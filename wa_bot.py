#!/usr/bin/python
from __future__ import print_function

import base64
import beanstalkc
import datetime
import fileinput
import time
import json
import threading

from Yowsup.connectionmanager import YowsupConnectionManager

info = error = lambda x: None

class Timestamp():
    def __init__(self, ms_str = None, ms_int = None):
        if ms_str is not None:
            self.time = long(ms_str) / 1000. / 1000.
        elif ms_int is not None:
            self.time = ms_int / 1000. / 1000.
        else:
            self.time = time.time()  #long
    def to_human_str(self):
        timestamp = self.time
        dtime = datetime.datetime.fromtimestamp(timestamp)
        return dtime.strftime("%Y-%m-%d %H:%M:%S.%f")
    def __str__(self):
        return str(self.ms_int())
    def ms_int(self):
        return int(round(self.time * 1000 * 1000))
    def __eq__(self, other):
        return self.time == other.time
    def __ne__(self,other):
        return self.time != other.time
    def __lt__(self, other):
        return self.time < other.time
    def __gt__(self, other):
        return self.time > other.time
    def __le__(self, other):
        return self.time <= other.time
    def __ge__(self, other):
        return self.time >= other.time

def catch_them_all(function):
    """ Decorator that logs all exceptions thrown by the called function, and allows to continue.
    Can be useful when used as wrapper for callbacks whose execution you don't fully control """
    def wrapper(*args, **kwargs):
        #try:
        return function(*args, **kwargs)
        #except:
        #    error("Exception in function %s" % function.__name__)
    return wrapper

class WAInterface(threading.Thread):
    def __init__(self, username, password, stopped_handler):
        threading.Thread.__init__(self)
        
        self.beanstalk_send = beanstalkc.Connection(host='localhost', port=14711)
        self.beanstalk_recv = beanstalkc.Connection(host='localhost', port=14711)
        self.beanstalk_send.watch('send')
        self.beanstalk_recv.use('recv')

        self.connected = False
        self.must_run = True
        self.msg_handler = lambda x: self.beanstalk_recv.put(json.dumps(x))
        self.stopped_handler = stopped_handler
        self.username = username
        self.password = base64.b64decode(password)
        self.cm = YowsupConnectionManager()
        self.cm.setAutoPong(True)
        self.signalsInterface = self.cm.getSignalsInterface()
        self.methodsInterface = self.cm.getMethodsInterface()
        self.signalsInterface.registerListener("notification_groupPictureUpdated", self.onGroup_PictureUpdated)
        self.signalsInterface.registerListener("group_gotPicture", self.onGroup_PictureGot)
        self.signalsInterface.registerListener("group_imageReceived", self.onGroup_ImageReceived)
        self.signalsInterface.registerListener("image_received", self.onImageReceived)
        self.signalsInterface.registerListener("group_videoReceived", self.onGroup_VideoReceived)
        self.signalsInterface.registerListener("videoimage_received", self.onVideoReceived)
        self.signalsInterface.registerListener("message_received", self.onMessageReceived)
        self.signalsInterface.registerListener("group_messageReceived", self.onGroup_MessageReceived)
        self.signalsInterface.registerListener("auth_success", self.onAuthSuccess)
        self.signalsInterface.registerListener("auth_fail", self.onAuthFailed)
        self.signalsInterface.registerListener("disconnected", self.onDisconnected)
        self.signalsInterface.registerListener("receipt_messageSent", self.onMessageSent)
        self.signalsInterface.registerListener("receipt_messageDelivered", self.onMessageDelivered)
        self.signalsInterface.registerListener("ping", self.onPing)
    
    @catch_them_all
    def onMessageReceived(self, messageId, jid, messageContent, timestamp, wantsReceipt, pushName, _):
        message = dict(kind="message", 
                       jid=jid, 
                       chan=self.username,
                       msg=messageContent,
                       pushName=pushName.decode('utf8', 'ignore'))
        message['time'] = str(Timestamp(ms_int = timestamp*1000))
        self.msg_handler(message)
        sendReceipts = True
        if wantsReceipt and sendReceipts:
            self.wait_connected()
            self.methodsInterface.call("message_ack", (jid, messageId))
    
    @catch_them_all
    def onImageReceived(self, messageId, jid, preview, url, size, receiptRequested, _):
        message = dict(kind="image", 
                       jid=jid, 
                       chan=self.username, 
                       msg=url)
        self.msg_handler(message)
        sendReceipts = True
        if receiptRequested and sendReceipts:
            self.wait_connected()
            self.methodsInterface.call("message_ack", (jid, messageId))
    
    @catch_them_all
    def onGroup_PictureUpdated(self, jid, author, timestamp, messageId, pictureId, receiptRequested):
        self.methodsInterface.call("group_getPicture", (jid,))
        sendReceipts = True
        if receiptRequested and sendReceipts:
            self.wait_connected()
            self.methodsInterface.call("message_ack", (jid, messageId))
    
    @catch_them_all
    def onGroup_PictureGot(self, jid, filePath):
        #TODO: upload filePath to services like imgur (or even upload as whatsapp picture, hah!) instead of displaying local file path here:
        message = dict(kind="group_picture", 
                       jid="unknown", 
                       chan=jid, 
                       msg=filePath)
        self.msg_handler(message)
    
    @catch_them_all
    def onGroup_ImageReceived(self, messageId, jid, author, preview, url, size, receiptRequested):
        message = dict(kind="group_image", 
                       jid=author, 
                       chan=jid, 
                       msg=url)
        self.msg_handler(message)
        sendReceipts = True
        if receiptRequested and sendReceipts:
            self.wait_connected()
            self.methodsInterface.call("message_ack", (jid, messageId))
    
    @catch_them_all
    def onVideoReceived(self, messageId, jid, preview, url, size, receiptRequested):
        message = dict(kind="video", 
                       jid=jid, 
                       chan=self.username, 
                       msg=url)
        self.msg_handler(message)
        sendReceipts = True
        if receiptRequested and sendReceipts:
            self.wait_connected()
            self.methodsInterface.call("message_ack", (jid, messageId))
    
    @catch_them_all
    def onGroup_VideoReceived(self, messageId, jid, author, preview, url, size, receiptRequested):
        message = dict(kind="group_video", 
                       jid=author, 
                       chan=jid, 
                       msg=url)
        self.msg_handler(message)
        sendReceipts = True
        if receiptRequested and sendReceipts:
            self.wait_connected()
            self.methodsInterface.call("message_ack", (jid, messageId))
    
    @catch_them_all
    def onGroup_MessageReceived(self, messageId, jid, author, messageContent, timestamp, wantsReceipt, pushName):
        message = dict(kind="group_message", 
                       jid=author,
                       chan=jid, 
                       msg=messageContent,
                       pushName=pushName.decode('utf8','ignore'))
        message['time'] = str(Timestamp(ms_int = timestamp*1000))
        self.msg_handler(message)
        sendReceipts = True
        if wantsReceipt and sendReceipts:
            self.wait_connected()
            self.methodsInterface.call("message_ack", (jid, messageId))

    @catch_them_all
    def run(self):
        try:
            info("Connecting as %s" %self.username)
            self.must_run = True
            self.methodsInterface.call("auth_login", (self.username, self.password))
            self.wait_connected()
            info("Connected as %s" %self.username)
            while self.must_run:
                if not self.connected:
                    self.methodsInterface.call("auth_login", (self.username, self.password))
                
                job = self.beanstalk_send.reserve(timeout=5)
                if job is not None:
                    data = json.loads(job.body)
                    try:self.send(data['target'], data['text'])
                    except:pass
                    job.delete()

        finally:
            info("Main loop closing")
            self.connected = False
            self.stopped_handler()
            self.must_run = False
    
    def stop(self):
        self.must_run = False
    
    def send(self, target, text):
        self.wait_connected()
        self.methodsInterface.call("message_send", (target, text.encode("utf-8")))
        info((" >>> WA %s: %s" %(target, text)).encode("utf-8"))
    
    @catch_them_all
    def onAuthSuccess(self, username):
        info("Authed %s" % username)
        self.connected = True
        self.methodsInterface.call("ready")
    
    @catch_them_all
    def onAuthFailed(self, username, reason):
        info("Auth Failed: %s" %reason)
        self.connected = False
    @catch_them_all
    def onDisconnected(self, reason):
        info("Disconnected because %s" %reason)
        self.connected = False
    
    @catch_them_all
    def onMessageSent(self, jid, messageId):
        info("Message successfully sent to %s" % jid)
    
    @catch_them_all
    def onMessageDelivered(self, jid, messageId):
        info("Message successfully delivered to %s" %jid)
        self.wait_connected()
        self.methodsInterface.call("delivered_ack", (jid, messageId))
    
    @catch_them_all
    def onPing(self, pingId):
        info("Pong! (%s)" %pingId)
        self.wait_connected()
        self.methodsInterface.call("pong", (pingId,))
    
    def wait_connected(self):
        while not self.connected:
            if not self.must_run:
                raise Exception("bot does not intend to connect")
            time.sleep(0.1)

def main():
    try:
        with open('config') as configfile:
            config = json.load(configfile)
            username = config['username']
            password = config['password']
            bot = WAInterface(username, password, lambda: print("stopped"))
            bot.run()
    except: pass

if __name__ == "__main__":
    while True: 
        time.sleep(1)
        main()


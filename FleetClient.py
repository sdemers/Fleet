#!/usr/bin/env python

import socket#for sockets
import sys #for exit
import time

try:
    #create an AF_INET, STREAM socket (TCP)
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
except socket.error, msg:
    print 'Failed to create socket. Error code: ' + str(msg[0]) + ' , Error message : ' + msg[1]
    sys.exit();

print 'Socket Created'

host = 'localhost'
port = 5000

try:
    remote_ip = socket.gethostbyname(host)

except socket.gaierror:
    #could not resolve
    print 'Hostname could not be resolved. Exiting'
    sys.exit()

print 'Ip address of ' + host + ' is ' + remote_ip

socketInfo = (remote_ip, port)
# Connect to remote server
s.connect(socketInfo)


def sendMessage(sock, sockInfo, msg):
    try :
        # Set the whole string
        sock.sendall(msg)
    except socket.error:
        # Send failed
        print 'Send failed'
        sys.exit()

#Send some data to remote server
newPilotMessage = "{\"systemMessage\":{\"body\":{\"newPlayer\":{\"name\":\"Serge\",\"type\":\"pilot\",\"frequency\":123.4}}}}"
initPosMessage = "{\"playerMessage\":{\"frequency\":123.4,\"recipients\":[\"Serge\"],\"body\":{\"initialPosition\":{\"position\":{\"x\":1,\"y\":2,\"z\":3}}}}}"

sendMessage(s, socketInfo, newPilotMessage)
time.sleep(2.0)
sendMessage(s, socketInfo, initPosMessage)

print 'Message send successfully'

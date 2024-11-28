#!/usr/bin/env python
# the code taken from example
# https://www.learnopencv.com/barcode-and-qr-code-scanner-using-zbar-and-opencv/

#import numpy as np
from __future__ import print_function
import pyzbar.pyzbar as pyzbar
import cv2
import time

count = 0
def decode(im):
    # find barcodes and qr codes
    decodedObjects = pyzbar.decode(im)
    global count

    # print results
    for obj in decodedObjects:
        print("Type: ", obj.type)
        print("Data: ", obj.data)
        print("Time: ", time.strftime("%d-%b-%YT%H:%M:%S%Z"))

    if len(decodedObjects) != 0:
        print("Scan count: ", count)
        count += 1

    return decodedObjects

import signal
import sys

running = True
def signal_handler(sig, frame):
    print("stop running!")
    global running
    running = False

if __name__ == "__main__":
    camera = cv2.VideoCapture(0)
    camera.set(3, 1280)
    camera.set(4, 720)
    #camera.set(cv2.CAP_PROP_FRAME_WIDTH, 1280)
    #camera.set(cv2.CAP_PROP_FRAME_HEIGHT, 720)
    signal.signal(signal.SIGINT, signal_handler)
    thres = 127
    while running:
        time.sleep(0.3)
        retval, image = camera.read()
        bw_image = cv2.threshold(image, thres, 255, cv2.THRESH_BINARY)[1]
        decode(bw_image)
    del(camera)

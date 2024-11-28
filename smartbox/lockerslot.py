#!/usr/bin/env python

import setup_pin as spin
import qrcode_scanner as qr
import decrypt_paxpobox as dc
import read_scanner as rs

import pyzbar.pyzbar as pyzbar
import cv2
import time
import RPi.GPIO as GPIO
import json
from PIL import Image
from picamera import PiCamera
from io import BytesIO
import threading
from Queue import Queue

import os
import sys
import signal

killed = False
running = True
pinShutdown = False

def stopRunning(sigcode, frame):
    print "Killed gracefully"
    global killed
    global running
    global pinShutdown
    killed = True
    running = False
    pinShutdown = True

def actSlot(data):
    obj = json.loads(dc.contentExtract(dc.decryptRsa(data)))
    print obj
    if not spin.isSlotOpened:
        if obj["number"] == "1":
            spin.anyslot(1)
        elif obj["number"] == "2":
            spin.anyslot(2)
    else:
        print "Slot already opened, please close first"

def decode(im):
    # find barcodes and qr codes
    decodedObjects = pyzbar.decode(im)

    # print results
    for obj in decodedObjects:
        print "Type: ", obj.type
        print "Data: ", obj.data
        print "Time: ", time.strftime("%d-%b-%YT%H:%M:%S%Z")

    if len(decodedObjects) > 0:
        theobj = decodedObjects[0]
        actSlot(theobj.data)

def setup():
    camera = PiCamera()
    camera.resolution = (1280, 720)
    camera.start_preview()
    time.sleep(2) # warm-up
    camera.shutter_speed = camera.exposure_speed
    camera.exposure_mode = 'off'
    g = camera.awb_gains
    camera.awb_mode = 'off'
    camera.awb_gains = g
    image = BytesIO()
    return camera, image

def readCamera(cam, imgstream):
    global running
    global pinShutdown
    global killed
    while running:
        try:
            time.sleep(0.3)
            """
            retval, image = camera.read()
            if retval:
                decode(image)
            else:
                print "cannot read camera image"

            """
            if debug: spin.printInputState()
            camera.capture(imgstream, format="jpeg")
            imgstream.seek(0)
            image = Image.open(imgstream)
            decode(image)
            imgstream = BytesIO()
            spin.checkOpenedSlot()
            if killed:
                print "killed!"
                running = False
                break

            if spin.shutdown():
                print "shutdown from pin!"
                running = False
                pinShutdown = True
                break

        except KeyboardInterrupt:
            running = False
            break

        except Exception, e:
            print "readCamera exception!"
            print str(e)

def readScanner():
    global running
    global killed
    global pinShutdown

    dev = rs.initDevice(0)
    if dev == None:
        return
    buf = ""
    while running:
        try:

            buf = rs.readScanner(dev)
            """
            chunk = ""
            shifted = False
            while chunk != 'finished':
                chunk, shifted = rs.readOne(dev, shifted)
                if chunk != None and not shifted:
                    buf += chunk
            """
            if buf == "" or buf == None:
                buf = ""
            else:
                actSlot(buf)

            spin.checkOpenedSlot()
            if killed:
                print "Killed in readScanner"
                running = False
                return

            if spin.shutdown():
                killed = True
                running = False
                pinShutdown = True
                return

        except KeyboardInterrupt:
            running = False
            break

        except Exception, e:
            print "readScanner exception!"
            print str(e)
            #running = False
            #break


if __name__ == "__main__":
    debug = False
    if "debug" in [x.lower() for x in sys.argv]:
        debug = True
    #running = True
    killed = False
    if os.system("sudo modprobe bcm2835-v4l2") != 0:
        running = False

    signal.signal(signal.SIGTERM, stopRunning)
    spin.setup()

    camera, imgstream = setup()
    #camera = cv2.VideoCapture(0)
    #camera.set(3, 1280)
    #camera.set(4, 720)
    #camera.set(cv2.cv.CV_CAP_PROP_FPS, 30)
    #camera.set(cv2.CAP_PROP_FRAME_WIDTH, 1280)
    #camera.set(cv2.CAP_PROP_FRAME_HEIGHT, 720)
    if running:
        print "Running the program"
        threads = [
                threading.Thread(target=readCamera, name="readCamera",
                    args=[camera, imgstream]),
                threading.Thread(target=readScanner, name="readScanner",
                    args=[])
                ]
        for thread in threads:
            thread.start()

        for thread in threads:
            thread.join()


    GPIO.cleanup()
    camera.close()
    #del(camera)
    #camera.release()
    if pinShutdown:
        os.system("sudo shutdown -h now")

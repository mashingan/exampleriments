#!/usr/bin/env python

# initial example is taken from
# https://makezine.com/projects/tutorial-raspberry-pi-gpio-pins-and-python/
import RPi.GPIO as GPIO
import time

LEDPIN = 18
SLOT1_OUT = 11
SLOT2_OUT = 13
SLOT1_IN = 15
SLOT2_IN = 16 # temporarily LEDOUT
SHUTDOWN_PIN = 37

isSlotOpened = False

def slotClose(channel):
    print "Slot closed! at ", now()
    global isSlotOpened
    isSlotOpened = False
    ledOff()

def setup():
    GPIO.setmode(GPIO.BOARD)

    # pin 11 is slot 1 in
    # pin 13 is slot 2 out
    # pin 15 is slot 1 in
    # pin 16 is slot 2 in
    # pin 18 is led
    GPIO.setup(SLOT1_OUT, GPIO.OUT, initial=GPIO.HIGH)
    GPIO.setup(SLOT2_OUT, GPIO.OUT, initial=GPIO.HIGH)
    GPIO.setup(SLOT1_IN, GPIO.IN, pull_up_down = GPIO.PUD_DOWN)
    #GPIO.setup(SLOT2_IN, GPIO.IN, pull_up_down = GPIO.PUD_DOWN)
    GPIO.setup(SLOT2_IN, GPIO.OUT, initial=GPIO.LOW)
    GPIO.setup(LEDPIN, GPIO.OUT, initial=GPIO.LOW)
    GPIO.setup(SHUTDOWN_PIN, GPIO.IN, pull_up_down = GPIO.PUD_UP)
    
    #GPIO.add_event_detect(SLOT1_IN, GPIO.RISING, callback=slotClose, bouncetime=300)
    #GPIO.add_event_detect(SLOT2_IN, GPIO.RISING, callback=slotClose, bouncetime=300)
    ledOff()

def shutdown():
    if GPIO.input(SHUTDOWN_PIN) == GPIO.LOW:
        return True
    else:
        return False

def ledOn():
    GPIO.output(LEDPIN, GPIO.HIGH)
    GPIO.output(SLOT2_IN, GPIO.LOW)

def ledOff():
    GPIO.output(LEDPIN, GPIO.LOW)
    GPIO.output(SLOT2_IN, GPIO.HIGH)

def checkOpenedSlot():
    global isSlotOpened
    #printInputState()
    if GPIO.input(SLOT1_IN) == GPIO.LOW and isSlotOpened:
        slotClose(1)
    #else:
        #isSlotOpened = True
        #ledOn()

def now():
    return time.strftime("%d-%b-%YT%H:%M:%S%Z")

def anyslot(slot):
    global isSlotOpened
    if isSlotOpened:
        print "There's still slot opening cannot open slot ", slot
        return
    print "Slot ", slot, " opened at ", now()
    isSlotOpened = True
    pin = 0
    if slot == 1:
        pin = SLOT1_OUT
    elif slot == 2:
        pin = SLOT2_OUT
    GPIO.output(pin, GPIO.LOW)
    time.sleep(0.3)
    GPIO.output(pin, GPIO.HIGH)
    ledOn()

def printInputState():
    print "GPIO Slot 1: ", GPIO.input(SLOT1_IN)


if __name__ == "__main__":
    print("Ready to listen pinout!")

    running = True
    setup()
    while running:
        try:
            if not isSlotOpened:
                anyslot(1)
            time.sleep(10)
        except KeyboardInterrupt:
            running = False

    GPIO.cleanup()

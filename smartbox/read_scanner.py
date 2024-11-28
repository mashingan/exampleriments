#!/usr/bin/env python

# 05e0:1200

import evdev
from evdev import InputDevice, categorize, ecodes
 
#dev = InputDevice("/dev/input/event0")

CODE_MAP_CHAR = {
        'KEY_MINUS': "-", 'KEY_SPACE': " ", 'KEY_U': "U",
        'KEY_W': "W",
        'KEY_BACKSLASH': "\\",
        'KEY_GRAVE': "`",
        'KEY_NUMERIC_STAR': "*",
        'KEY_NUMERIC_3': "3",
        'KEY_NUMERIC_2': "2",
        'KEY_NUMERIC_5': "5",
        'KEY_NUMERIC_4': "4",
        'KEY_NUMERIC_7': "7",
        'KEY_NUMERIC_6': "6",
        'KEY_NUMERIC_9': "9",
        'KEY_NUMERIC_8': "8",
        'KEY_NUMERIC_1': "1",
        'KEY_NUMERIC_0': "0",
        'KEY_E':"E",
        'KEY_D': "D",
        'KEY_G': "G",
        'KEY_F': "F",
        'KEY_A': "A",
        'KEY_C': "C",
        'KEY_B': "B",
        'KEY_M': "M",
        'KEY_L': "L",
        'KEY_O': "O",
        'KEY_N': "N",
        'KEY_I': "I",
        'KEY_H': "H",
        'KEY_K': "K",
        'KEY_J': "J",
        'KEY_Q': "Q",
        'KEY_P': "P",
        'KEY_S': "S",
        'KEY_X': "X",
        'KEY_Z': "Z",
        'KEY_KP4': "4",
        'KEY_KP5': "5",
        'KEY_KP6': "6",
        'KEY_KP7': "7",
        'KEY_KP0': "0",
        'KEY_KP1': "1",
        'KEY_KP2': "2",
        'KEY_KP3': "3",
        'KEY_KP8': "8",
        'KEY_KP9': "9",
        'KEY_5': "5",
        'KEY_4': "4",
        'KEY_7': "7",
        'KEY_6': "6",
        'KEY_1': "1",
        'KEY_0': "0",
        'KEY_3': "3",
        'KEY_2': "2",
        'KEY_9': "9",
        'KEY_8': "8",
        'KEY_LEFTBRACE': "[",
        'KEY_RIGHTBRACE': "]",    
        'KEY_COMMA': ",",
        'KEY_EQUAL': "=",    
        'KEY_SEMICOLON': ";",
        'KEY_APOSTROPHE': "'",
        'KEY_T': "T",
        'KEY_V': "V",
        'KEY_R': "R",
        'KEY_Y': "Y",
        'KEY_TAB': "\t",
        'KEY_DOT': ".",
        'KEY_SLASH': "/",
}

def enumerateDevices():
    devices = [InputDevice(fn) for fn in evdev.list_devices()]
    for device in devices:
        print "\t{}\t{}".format(device.fn, device.name)


def initDevice(eventId):
    try:
        return InputDevice("/dev/input/event{}".format(eventId))
    except Exception, e:
        print str(e)
        return None

def transform(scancode, shifted):
    res = ecodes.KEY[scancode]
    if scancode == ecodes.KEY_ENTER:
        return ('finish', False)
    elif scancode == ecodes.KEY_LEFTSHIFT:
        return ('', True)

    if shifted:
        if thekey == 'KEY_3':
            return ('#', False)
        elif thekey == 'KEY_EQUAL':
            return ('+', False)
        else:
            return (CODE_MAP_CHAR.get(thekey), False)
    else:
        addkey = ord(CODE_MAP_CHAR.get(thekey))
        if ord('A') <= addkey <= ord('Z'):
            addkey += 32
        return (chr(addkey), False)

def readOne(dev, shifted):
    buf = ""
    event = dev.read_one()
    if event == None:
        return (None, False)
    if event.type == evdev.ecodes.EV_KEY:
        data = categorize(event)
        if data.keystate == 1:
            return transform(data.scancode, shifted)

def readScanner(dev):
    buf = ""
    shifted = False
    for event in dev.read_loop():
        #print event
        if event.type == evdev.ecodes.EV_KEY:
            data = categorize(event)
            thekey = ecodes.KEY[data.scancode]
            if data.keystate == 1: # if the event is key down
                #print repr(data)
                #print data
                #print CODE_MAP_CHAR.get(thekey)
                if data.scancode == ecodes.KEY_ENTER:
                    break
                elif data.scancode == ecodes.KEY_LEFTSHIFT:
                    shifted = True
                    continue

                if shifted:
                    if thekey == 'KEY_3':
                        buf += '#'
                    elif thekey == 'KEY_EQUAL':
                        buf += '+'
                    else:
                        buf += CODE_MAP_CHAR.get(thekey)
                else:
                    addkey = ord(CODE_MAP_CHAR.get(thekey))
                    if ord('A') <= addkey <= ord('Z'):
                        addkey += 32
                    buf += chr(addkey)

                shifted = False
    return buf

if __name__ == '__main__':

    result = "#DATA#BmRenmtM9IhyKLMKGlJKgKX2l/QSSbGR1y9P2mtzlL7UQuM8VH0/OH5Ba/Sa6DDx83PPR4mc9GBsnZ1tTJNrfsp4tJupjs3grTVdij3+x+ICnGczXlYdE8qsQbzX8zzhj8k+A7M7PpHuhpZnfGY3gOHouXMqLtPPP+XWbcPvkhfDHPJbvjQmF4vpvQsBFPKQNCwnDP3sY8zC3jmY8YJ61+24XM/Q4WwIoLMLZMkytMWys55Ym1eUfrhYlnX9zNPOUIMXNKQYsRVV+CABkOm8FhUgIjaGpPF0Cww0kiRuCHrtxmnqK3IzpjXmkelQTJjb/IRGlEQ8SFk/m0MyTtQE2Q==#DATA#"
    dev = initDev(0)
    buf = readScanner(dev)
    print "result: ", buf
    print "is result correct? ", buf == result

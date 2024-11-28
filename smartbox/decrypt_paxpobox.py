#!/usr/bin/env python

from Crypto.PublicKey import RSA
from Crypto.Cipher import PKCS1_OAEP
from Crypto.Cipher import PKCS1_v1_5 as Cipher_PKCS1_v1_5
from base64 import b64decode, b64encode
import zlib
import json
import time

PUBLICKEY = """-----BEGIN RSA PUBLIC KEY-----
-----END RSA PUBLIC KEY-----"""

PRIVATEKEY = """-----BEGIN RSA PRIVATE KEY-----
-----END RSA PRIVATE KEY-----"""

data = ""

def msgPadding(msg):
    dataobj = zlib.compressobj(zlib.Z_DEFAULT_COMPRESSION, zlib.DEFLATED,
            zlib.MAX_WBITS+16)
    data = dataobj.compress(msg) + dataobj.flush()
    encoded = b64encode(data)
    print "encoded: ", encoded
    return "OPEN|" + encoded + "|" + str(int(round(time.time() * 1000)))

def encrytRsa(msg):
    recpKey = RSA.importKey(PUBLICKEY)
    cipherRsa = Cipher_PKCS1_v1_5.new(recpKey)
    return "#DATA#" + b64encode(cipherRsa.encrypt(msg)) + "#DATA#"

def decryptRsa(data):
    data = data.strip("#DATA#")
    rawCipherData = b64decode(data)
    rsakey = RSA.importKey(PRIVATEKEY)
    cipher = Cipher_PKCS1_v1_5.new(rsakey)
    decrypted = cipher.decrypt(rawCipherData, None)
    return decrypted

def contentExtract(decrypted):
    print "decrypted: ", decrypted
    zipstr = b64decode(decrypted.split('|')[1])
    return zlib.decompress(zipstr, 16+zlib.MAX_WBITS)

if __name__ == "__main__":

    decrypted = decryptRsa(data)
    print decrypted
    content = contentExtract(decrypted)
    print content
    pyobj = json.loads(content)
    if pyobj["number"] == "1":
        print "opened slot 1"
    elif pyobj["number"] == "2":
        print "opened slot 2"
    else:
        print "unknown number ", pyobj["number"]

    slot2 = """{"number": "2"}"""
    print "to encrypt: ", slot2
    padded = msgPadding(slot2)
    print "padded: ", padded
    slot2enc = encrytRsa(padded)
    print slot2enc
    decrypted = decryptRsa(slot2enc)
    print decrypted
    print contentExtract(decrypted)

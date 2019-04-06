import sys
import subprocess
from threading  import Thread

try:
    from Queue import Queue, Empty
except ImportError:
    from queue import Queue, Empty  # python 3.x


def enqueue_output(out, queue):
    for line in iter(out.readline, b''):
        queue.put(line)
    out.close()


def getOutput(outQueue):
    outStr = ''
    try:
        while True: #Adds output from the Queue until it is empty
            outStr+=outQueue.get_nowait()

    except Empty:
        return outStr
try:
    p = subprocess.Popen("cmd", stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=False, universal_newlines=True)
except FileNotFoundError:
    p = subprocess.Popen("/bin/bash", stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True, universal_newlines=True)

outQueue = Queue()
errQueue = Queue()

outThread = Thread(target=enqueue_output, args=(p.stdout, outQueue))
errThread = Thread(target=enqueue_output, args=(p.stderr, errQueue))

outThread.daemon = True
errThread.daemon = True

outThread.start()
errThread.start()


# someInput = input("Input: ")
#
# p.stdin.write(someInput)
p.stdin.write("echo $test")

errors = getOutput(errQueue)
output = getOutput(outQueue)
print(output)

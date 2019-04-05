import pexpect

child = pexpect.spawn('/bin/bash')
child.sendline('echo test')
print(child.before, child.after,sep=",")
child.readline()
child.sendline('echo $PATH')
print(child.before, child.after,sep=",")
child.readline()
child.close()
print(str(child))

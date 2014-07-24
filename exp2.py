#!/usr/bin/python

import subprocess

class ScriptException(Exception):
    def __init__(self, returncode, stdout, stderr, script):
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr
        Exception.__init__('Error in script')
        
def run_bash(script):	
	proc = subprocess.Popen(['bash','-c',script],
		stdout = subprocess.PIPE, 
		stderr = subprocess.PIPE,
		stdin = subprocess.PIPE)
	stdout, stderr = proc.communicate()
	if proc.returncode:
		raise ScriptException(proc.returncode, stdout, stderr, script)
	return stdout, stderr

def main():
	remove_spaces = "find . -depth -name \'* *\' \
	 | while IFS= read -r f ; do mv -i \"$f\" \"$(dirname \"$f\")/$(basename \"$f\"|tr \' \' _)\" ; done"
	run_bash(remove_spaces)

if __name__ == "__main__":
	main()

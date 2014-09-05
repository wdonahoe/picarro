#!/usr/bin/python

import subprocess
import optparse
import os
import sys
# TODO: Add Windows functionality.

SCRIPT_NAME = "picarro.R"


class ScriptException(Exception):
    def __init__(self, returncode, stdout, stderr, script):
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr
        Exception.__init__('Error in script')


def run_bash(script):	
	proc = subprocess.Popen(['bash','-c',script], 
		stderr = subprocess.PIPE,
		stdin =  subprocess.PIPE)
	stdout, stderr = proc.communicate()
	if proc.returncode:
		raise ScriptException(proc.returncode, stdout, stderr, script)
	return stdout, stderr


# TODO: Add date ranges.
def run(foldername, filename, num_files):
	try:
		subprocess.call(["./" + SCRIPT_NAME] + [foldername, filename, num_files])
	except OSError as e:
		print "OS error({0}): {1}".format(e.errno, e.strerror)


def main():
	remove_spaces = "find . -depth -name \'* *\' \
	 | while IFS= read -r f ; do mv -i \"$f\" \"$(dirname \"$f\")/$(basename \"$f\"|tr \' \' _)\" ; done"

	usage = "usage: ./%s foldername experiment-data_filename [options]" % os.path.basename(sys.argv[0])
	parser = optparse.OptionParser(usage = usage)

	parser.add_option('-s','--spaces',action="store_true",dest="script",
		help="remove spaces from all files and folders in [foldername]",default=False)
	parser.add_option('-n','--numfiles',type="string",action="store",dest="num_files",
		help="number of files in each folder.",default='15')

	(options, args) = parser.parse_args()

	if len(args) < 2:
		parser.error("Incorrect number of arguments.\n Please specify a folder name and measurement file.")
	elif len(args) > 3:
		parser.error("Incorrect number of arguments.\n Too many arguments.")
	else:
		if options.script:
			run_bash(remove_spaces)
		run(args[0], args[1], options.num_files)


if __name__ == "__main__":
	main()

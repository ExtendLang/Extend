import sys
import re
import glob
import os

def fix_main(filename):
    out_file = open("testcases/inputs/{0}".format(filename),"w")
    for line in open("testcases/inputs_old/{0}".format(filename)):
        replaced_line = re.sub(r"^main\(\)(.*)$",r'main([1,n] args)\1',line)
        out_file.write(replaced_line)
    out_file.close()

if __name__ == "__main__":
    for f in os.listdir("testcases/inputs_old"):
        fix_main(f)

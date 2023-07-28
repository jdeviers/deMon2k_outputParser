import glob # for getting a filelist
import re   # for pattern matching
import csv
import sys


mask = "./" + str(sys.argv[1]) + "/*.out"
filelist = glob.glob(mask)

for infile in filelist:

  atlist = {}
  with open(infile, 'r') as f:
    for line in f:
      if re.match("\s\*\*\*\sATOM", line):

        # Find the atom name, makes it a key in the atlist dictionary, with for value an empty list.
        atname = re.search("\(\s(.*)\s\)",line).group(1)
        atlist[atname] = []
      
        # Skip 4 lines. No need for an iteration variable here.
        for _ in range(4):
          next(f)

        # Reads the tensors elements into a list, appending it to the previous one to make a list of lists.
        for _ in range(3):
          atlist[atname].append(list(next(f).split())[1:4])

    # Write tensors to N01.dat, N02.dat etc
    for k in atlist:
      outfile = str(k + '.csv')
      #with open(outfile,'a',newline="") as f2:
        #csv.writer(f2,delimiter=" ").writerows(atlist[k])


#  print(atlist)

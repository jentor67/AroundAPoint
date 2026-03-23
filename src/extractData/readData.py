#!/usr/bin/python3

fileSource = "/mnt/kdrive/data.txt"

iObject = 0

while iObject < 6 :
    with open("obj" + str(iObject) + ".txt", 'w') as file:
        i=0
        with open(fileSource, "r") as f:
            for line in f:
                if ((i-iObject)%10) == 0 :
                    file.write(line.strip() + '\n')
                i += 1
    iObject += 1

# -*- coding: utf-8 -*-
import midiutil as mu
import pandas
import numpy as np
melody = pandas.read_csv("melody.csv")
array=melody.to_numpy()
note=array[:,2]
note=note.astype(int)
dur=array[:,3]
track=0
channel=0
time=0
tempo=90
duration=1
volume=100
testvar=[(note[i],dur[i]) for i in range(0,len(note))]

MyMIDI=mu.MIDIFile(1)

MyMIDI.addTempo(track,time,tempo)

for pitch,y in testvar:
    MyMIDI.addNote(track,channel,pitch,time,y,volume)
    time=time+y
            
            
with open("melody.mid","wb") as output_file:
    MyMIDI.writeFile(output_file)
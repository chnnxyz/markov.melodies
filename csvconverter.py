# -*- coding: utf-8 -*-
import midiutil as mu
import pandas
import numpy as np
melody = pandas.read_csv("uwu.csv")
array=melody.to_numpy()
note=array[:,2]
dur=array[:,3]
track=0
channel=0
time=0
tempo=90
duration=1
dur2=np.cumsum(dur)
volume=100

MyMIDI=mu.MIDIFile(1)

MyMIDI.addTempo(track,time,tempo)
for x,pitch in enumerate(note):
        MyMIDI.addNote(track,channel,pitch,time+x,y,volume)
            
            
with open("test.mid","wb") as output_file:
    MyMIDI.writeFile(output_file)
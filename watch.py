from obspy import UTCDateTime
from obspy.clients.fdsn import Client
import datetime
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize
import numpy as np
import csv




##MAGNITUDES


no_of_days = int(input("How many days back from now are we looking?"))
minimum = float(input("Minimum magnitude of earthquakes desired?"))



# client = Client("IRIS")
client = Client(f"{input('Which data client are we looking at - GEONET or IRIS?:').upper()}")
cat = client.get_events(minmagnitude=max(minimum, 2.0), starttime=UTCDateTime.now() - no_of_days * 86400)  #do not want to waste time importing EQs below 2 due to bloat

#Import magnitude values to fit into exponential
mags = [eq.preferred_magnitude().mag for eq in cat]

print(f"Length of entries for magnitudes is {len(mags)}")

printpath = input("Name of file to make and save to:")

#Write out to csv
with open(rf'F:/Uni/Esci451/Assignment 2/{printpath}.csv', 'w', newline='') as f:
    thewriter = csv.writer(f)
    thewriter.writerow(['Magnitudes'])
    thewriter.writerow(mags)


##ARRIVAL TIMES



#Import time values to fit into Poisson
#Importing the entire catalog is meaningless
#We need to import between a range of magnitudes for any meaningful analysis of EQ occurrence analysis
starting_mag = int(input("We will import a range of magnitudes between your input here and 1 above. What shall it be?"))
no_of_days = int(input("How many days back from now are we looking?"))
client = Client(f"{input('Which data client are we looking at - GEONET or IRIS?:').upper()}")
new_cat = client.get_events(minmagnitude=starting_mag,maxmagnitude=starting_mag+1. , starttime=UTCDateTime.now() - no_of_days * 86400)
output = (new_cat.__str__(print_all=True))
output = output.split('\n')
output_=output[1:]

print(f"Length of entries for times is {len(output_)}")

times = [x.split('|')[0] for x in output_]

#Now we need to convert this to more workable times
UTC_times = [UTCDateTime(x) for x in times]
UTC_minimum = min(UTC_times)
UTC_delta_times = [x - UTC_minimum for x in UTC_times]  #this is to print events as an occurrence within the time frame


printpath = input("Name of file to make and save to:")


with open(rf'F:/Uni/Esci451/Assignment 2/Times/{printpath}.csv', 'w', newline='') as f:
    thewriter = csv.writer(f)
    thewriter.writerow(['Times'])
    thewriter.writerow(UTC_delta_times)



#Combo data



no_of_days = int(input("How many days back from now are we looking?"))
minimum = float(input("Minimum magnitude of earthquakes desired?"))



# client = Client("IRIS")
client = Client(f"{input('Which data client are we looking at - GEONET or IRIS?:').upper()}")
cat = client.get_events(minmagnitude=max(minimum, 2.0), starttime=UTCDateTime.now() - no_of_days * 86400)

#Import magnitude values to fit into exponential
mags = [eq.preferred_magnitude().mag for eq in cat]

print(f"Length of entries for magnitudes is {len(mags)}")

output = (cat.__str__(print_all=True))
output = output.split('\n')
output_=output[1:]
print(f"Length of entries for times is {len(output_)}")

times = [x.split('|')[0] for x in output_]

UTC_times = [UTCDateTime(x) for x in times]
UTC_minimum = min(UTC_times)
UTC_delta_times = [x - UTC_minimum for x in UTC_times]  #this is to print events as an occurrence within the time frame

printpath = input("Name of file to make and save to:")

#Write out to csv
with open(rf'F:/Uni/Esci451/Assignment 2/{printpath}.csv', 'w', newline='') as f:
    thewriter = csv.writer(f)
    thewriter.writerow(['Magnitudes'])
    thewriter.writerow(mags)
    thewriter.writerow(['Times'])
    thewriter.writerow(UTC_delta_times)









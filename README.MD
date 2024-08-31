# Lisps created with the help of ChatGPT with tweaks when it didn't work quite right.

## PolyLine Table - PLT

Inserts list of layers that have polylines and their lengths.
![alt text](images/image.png)
## Copy Layer with Prefix - CLPX

Copies selected layer/s with prompt for prefix.
![alt text](images/image-2.png)
![alt text](images/image-3.png)
## Copy Layer with Suffix - CLSX

Copies selected layer/s with prompt for suffix.  As above.

## Total Polyline Length Text - LTX

Select polylines to measure and the total length will be displayed in a text object.
The dwgunits is selected and added to the suffix.
This wil be printed in the command line, but the text defaults to meters.
![alt text](images/image-4.png)

## Block Field Count - BFC

Create a field with total count of selected block.
Use REA to update the field.

![alt text](images/image-5.png)

# Lisps used in custom toolbar

| Name                        | Command | Source                                                            |
|-----------------------------|---------|-------------------------------------------------------------------|
| Rotate Multiple Objects     | RMO     | https://www.cadforum.cz/en/download.asp?fileID=3577               |
| Text 2 Mtext                | T2M     | https://www.lee-mac.com/text2mtext.html                           |
| Incremental Numbering Suite | NUMINC  | https://www.lee-mac.com/numinc.html                               |
| EP_WIRE_V14-1.VLX           | EP_WIRE | https://sreekanthkadungil.blogspot.com/2024/03/photo-gallery.html |
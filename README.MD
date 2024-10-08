## Table of Contents

- [Lisps Original](#lisps-original)
  - [PolyLine Table - PLT](#polyline-table---plt)
  - [Copy Layer with Prefix - CLPX](#copy-layer-with-prefix---clpx)
  - [Copy Layer with Suffix - CLSX](#copy-layer-with-suffix---clsx)
  - [Total Polyline Length Text - LTX](#total-polyline-length-text---ltx)
  - [Block Field Count - BFC](#block-field-count---bfc)
- [Lisps used in custom toolbar](#lisps-used-in-custom-toolbar)
- [Lisp Sources](#lisp-sources)
  - [Websites](#websites)
  - [Telegram](#telegram)

# Lisps Original

Created with the help of ChatGPT with tweaks when it didn't work quite right.

| Name                       | Command |
|----------------------------|---------|
| PolyLine Table             | PLT     |
| Copy Layer with Prefix     | CLPX    |
| Copy Layer with Suffix     | CLSX    |
| Total Polyline Length Text | LTX     |
| Block Field Count          | BFC     |

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

| Name                        | Command  | Source                                                            |
|-----------------------------|----------|-------------------------------------------------------------------|
| Rotate Multiple Objects     | RMO      | https://www.cadforum.cz/en/download.asp?fileID=3577               |
| Text 2 Mtext                | T2M      | https://www.lee-mac.com/text2mtext.html                           |
| Incremental Numbering Suite | NUMINC   | https://www.lee-mac.com/numinc.html                               |
| EP_WIRE_V14-1.VLX           | EP_WIRE  | https://sreekanthkadungil.blogspot.com/2024/03/photo-gallery.html |
| Total Length                | TL       | https://payazed.wordpress.com/2015/06/02/tl/                      |
| PLDiet                      | PLD      | https://www.cadforum.cz/en/download.asp?fileID=3078               |
| Block Counter               | BLKCOUNT | https://www.lee-mac.com/blockcounter.html                         |
| Join Line                   | JL       | https://payazed.wordpress.com/2015/06/02/jl/                      |
| Block Legend                | BLEGEND  | https://www.cadforum.cz/en/download.asp?fileID=928                |
| Table Sort                  | TABLESORT|                                                                   |
| Select Similar              | SELS     | www.cadforum.cz                                                   |
| Incremental Numbering Suite | NUMINC   | https://www.lee-mac.com/                                          |
| Cable Schedule              | CABLE    | https://forum.dwg.ru/showthread.php?t=138086&page=1               |


# Lisp Sources

## Websites

| Name                 | Website                                                                                  |
|----------------------|------------------------------------------------------------------------------------------|
| Kims Projects        | https://www.kimprojects.com/free-lisp-programs/                                          |
| CADViet Forum        | https://www.cadviet.com/forum/index.php?app=forums&module=forums&controller=forums&id=50 |
| Jeffery P Sanders    | https://jefferypsanders.com/autolisp.html                                                |
| AfraLisp             | https://www.afralisp.net/autolisp/                                                       |
| Lee Mac              | https://www.lee-mac.com/programs.html                                                    |
| Russian              | https://forum.dwg.ru/                                                                    |
| The Swamp            | https://www.theswamp.org/                                                                |
| ChatGPT AutoLisp Ace | https://chatgpt.com/g/g-Zt0xFNpOH-autolisp-ace                                           |
| Cadalyst             | https://cadtips.cadalyst.com/                                                            |

## Telegram

| Name                  | Telegram                        |
|-----------------------|---------------------------------|
| AutoCAD Civil 3D Lisp | https://t.me/autocadcivil3dlisp |
| Yazan Hayani          | https://t.me/yazanhayani        |
| PTS CAD Expert        | https://t.me/ptscadexpert       |
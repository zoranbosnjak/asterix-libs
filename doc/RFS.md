# RFS mechanism

Regular cat002, edition 1.1 example datablock: `0x020007c0112201`, dissected:

```
0x
02     <- category
0007   <- length
c0     <- fspec bits
1122   <- item 010 (FRN 1)
01     <- item 000 (FRN 2)
```

Suppose we want to use RFS and send item 000 first, item 010 second... that would be:

```
0x
02     <- cagegory
000b   <- length
0102   <- fspec bits, now only RFS bit is set
02     <- 2 items are following
02     <- FRN 2 indicator (that is: item 000)
01     <- content of item 000
01     <- FRN 1 indicator (item 010)
1122   <- content of item 010
```

So, all together, the datablock in this case would look like:
`0x02000b0102020201011122`.

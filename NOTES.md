---
title:
author: "Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>"
tags:
---

# CALL tree jjms.sa

- jjms.sa(stk, idx)
 - jjms(stock, indices, dat, ctl)
  - buildjjmctl(stock, indices, ctl)
  - buildjjmdata(stock, indices, dat)
  - runjjms(mod, path)
    - jjmR::writeJJM(object, path)
    - exejjms(name, path)
      - ./jjms
  - readRep/readFLSjjm


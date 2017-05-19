#!/bin/bash
diff -Naur -X exclude.pats RegCM-4.5.0_org RegCM-4.5.0 >&regcm_4.5.0_COP.patch
#diff -Naur -X exclude.pats r6146_org r6146 >&regcm_r6146_COP.patch

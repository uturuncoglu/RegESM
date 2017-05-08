#Chapter 6: Limitations

## 6.1 Conservation of the exchange fields

In the current version of the coupled modeling system, a customized version of the conservation algorithm is implemented. In this case, the conservation algorithm ensures that the global integral of the source and destination fields (over the matched regions) will remain same but it does not guarantee local conservation of the exchange fields. It just applies the difference of the global integral of source and destination fields to the destination field across the domain.


**Note 1:** The first-order conservative interpolation technique, which is currently supported by ESMF, might create artifacts (square like shapes in the destination field) in the interpolation when the ration between grid resolution of components is high (i.e. 50 km ATM and 5-7km in **OCN**).

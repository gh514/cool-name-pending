Grid 9X9;

R1C4 = 3;
R1C9 = 5;
R2C3 = 8;
R2C4 = 3;
R2C5 = 10;
R2C8 = 5;
R3C2 = 3;
R3C6 = 4;
R3C7 = 4;
R4C1 = 1;
R4C2 = 3;
R4C4 = 3;
R4C7 = 2;
R5C2 = 2;
R5C5 = 3;
R5C8 = 2;
R6C3 = 2;
R6C6 = 3;
R6C8 = 1;
R6C9 = 3;
R7C3 = 4;
R7C4 = 4;
R7C8 = 3;
R8C2 = 4;
R8C5 = 4;
R8C6 = 3;
R8C7 = 3;
R9C1 = 6;
R9C6 = 1;

Forall Cells c.(
    c = c.Region.Size
);

Forall Regions r1, r2.(
    r1 Adj r2 -> r1.Size != r2.Size
)
Grid 9X9;

R1C1 = 5;
R1C2 = 3;
R1C5 = 7;
R2C1 = 6;
R2C4 = 1;
R2C5 = 9;
R2C6 = 5;
R3C2 = 9;
R3C3 = 8;
R3C8 = 6;
R4C1 = 8;
R4C5 = 6;
R4C9 = 3;
R5C1 = 4;
R5C4 = 8;
R5C6 = 3;
R5C9 = 1;
R6C1 = 7;
R6C5 = 2;
R6C9 = 6;
R7C2 = 6;
R7C7 = 2;
R7C8 = 8;
R8C4 = 4;
R8C5 = 1;
R8C6 = 9;
R8C9 = 5;
R9C5 = 8;
R9C8 = 7;
R9C9 = 9;

Cells Contain Digits 1 To 9;

Box box1 = [R1C1 To R3C3];
Box box2 = [R1C4 To R3C6];
Box box3 = [R1C7 To R3C9];
Box box4 = [R4C1 To R6C3];
Box box5 = [R4C4 To R6C6];
Box box6 = [R4C7 To R6C9];
Box box7 = [R7C1 To R9C3];
Box box8 = [R7C4 To R9C6];
Box box9 = [R7C7 To R9C9];
  
Cells In Rows Are Distinct;
Cells In Columns Are Distinct;
Cells In Boxes Are Distinct;


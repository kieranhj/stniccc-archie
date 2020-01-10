.equ OS_WriteC, 0
.equ OS_WriteO, 2
.equ OS_NewLine, 3
.equ OS_Byte, 6
.equ OS_Word, 7
.equ OS_File, 8
.equ OS_Exit, 0x11
.equ OS_BreakPt, 0x17
.equ OS_ChangeDynamicArea, 0x2a
.equ OS_GenerateError, 0x2b
.equ OS_ReadVduVariables, 0x31
.equ OS_ReadMonotonicTime, 0x42
.equ OS_ReadDynamicArea, 0x5c
.equ OS_ConvertHex8, 0xd4
.equ OS_ConvertCardinal4, 0xd8	

.equ OSByte_Vsync, 19
.equ OSByte_WriteVDUBank, 112
.equ OSByte_WriteDisplayBank, 113
.equ OSByte_ReadKey, 129

.equ OSWord_WritePalette, 12

.equ IKey_LeftClick, 0xf6
.equ IKey_RightClick, 0xf4
.equ IKey_Space, 0x9d

.equ DynArea_Screen, 2

.equ VD_ScreenStart, 148 

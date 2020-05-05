if x%1 == xrelease cl /W3 /wd4146 /D_CRT_SECURE_NO_WARNINGS /O2 /Fe:toc.exe main.c
if x%1 == x cl /DTOC_DEBUG /W3 /wd4146 /D_CRT_SECURE_NO_WARNINGS /Od /Fe:toc.exe /DEBUG /Zi main.c

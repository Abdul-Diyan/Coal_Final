[org 0x0100]

jmp start


;;;;;;;;;            declaring variables         ;;;;;;;;;;;;;;;;;


name1: db'Score'
length1: dw 5
name2: db'Time'
length2: dw 4
name3: db'Your score is: '
length3: dw 15
name4: db'Next Block'
length4: dw 10
name5: db'Game Over'
length5: dw 9
rand: dw 0
randnum: dw 0

name6: db 'Project by: '
length6: dw 12
name7: db 'Abdul-Diyan (22-6789)  &  Ruban Ahmed (22L-6848).'
length7: dw 49
name8: db 'Instructions: '
length8: dw 14
name9: db '1- Press D for down movement.'
length9: dw 29
name10: db '2- Press F for right movement.'
length10: dw 30
name11: db '3- Press S for left movement.'
length11: dw 29
name12: db '4- Time for playing game is: 5 mins / 300 secs. '
length12: dw 48

scoreflag: dw 0
scroeeee: dw 0

message1: db'Press Enter to Continue the game.'
lengthm: dw 33
move: dw 10
terminate: dw 100


gameover: dw 0
sec: dw 3200
sec1: dw 300
tickcount: dw 0
oldisr: dd 0

nextshape1: dw 0
currentshape: dw 0

x1: dw 0
x2: dw 0
x3: dw 0
x4: dw 0 
print: dw 0



;;;;;;;;;;;;  function to print total score at gameover screen ;;;;;;;;;;;;;;



printnum2:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov ax, [bp+4] ; load number in ax
    mov bx, 10 ; use base 10 for division
    mov cx, 0 ; initialize count of digits

nextdigitnum10:
    mov dx, 0 ; zero upper half of dividend
    div bx ; divide by 10
    add dl, 0x30 ; convert digit into ascii value
    push dx ; save ascii value on stack
    inc cx ; increment count of values
    cmp ax, 0 ; is the quotient zero
    jnz nextdigitnum10 ; if no divide it again

    mov di,2490; point di to 79th column
nextposnum10:
    pop dx ; remove a digit from the stack
    mov dh, 0x70 ; use normal attribute
    mov [es:di], dx ; print char on screen
    add di, 2 ; move to previous screen location
    loop nextposnum10 ; repeat for all digits on stack

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 2



;;;;;;;;;;;;  function to print after each loop score   ;;;;;;;;;;;;;;



printnum1: 
	 push bp 
	 mov bp, sp 
	 push es 
	 push ax 
	 push bx 
	 push cx 
	 push dx 
	 push di 
	 mov ax, 0xb800 
	 mov es, ax ; point es to video base 
	 mov ax, [bp+4] ; load number in ax 
	 mov bx, 10 ; use base 10 for division 
	 mov cx, 0 ; initialize count of digits 
	nextdigit1: mov dx, 0 ; zero upper half of dividend 
	 div bx ; divide by 10 
	 add dl, 0x30 ; convert digit into ascii value 
	 push dx ; save ascii value on stack 
	 inc cx ; increment count of values 
	 cmp ax, 0 ; is the quotient zero 
	 jnz nextdigit1 ; if no divide it again 
	 mov di, 1084
	 nextpos1: pop dx ; remove a digit from the stack 
	 mov dh, 0x70 ; use normal attribute 
	 mov [es:di], dx ; print char on screen 
	 add di, 2 ; move to next screen location 
	 loop nextpos1 ; repeat for all digits on stack
	 pop di 
	 pop dx 
	 pop cx 
	 pop bx 
	 pop ax 
	 pop es 
	 pop bp 
	 ret 2 
 
 



;;;;;;;;;;;;  function to print time    ;;;;;;;;;;;;;;



printnum:
    push bp
    mov bp, sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, 0xb800
    mov es, ax ; point es to video base
    mov ax, [bp+4] ; load number in ax
    mov bx, 10 ; use base 10 for division
    mov cx, 0 ; initialize count of digits

nextdigit:
    mov dx, 0 ; zero upper half of dividend
    div bx ; divide by 10
    add dl, 0x30 ; convert digit into ascii value
    push dx ; save ascii value on stack
    inc cx ; increment count of values
    cmp ax, 0 ; is the quotient zero
    jnz nextdigit ; if no divide it again

    mov di, 2042  ; point di to 79th column
nextpos:
    pop dx ; remove a digit from the stack
    mov dh, 0x70 ; use normal attribute
    mov [es:di], dx ; print char on screen
    add di, 2 ; move to previous screen location
    loop nextpos ; repeat for all digits on stack
	mov word[es:di], 0x7000
	add di, 2
	mov word[es:di], 0x7000

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 2




;;;;;;;;;;;;  intrupt for timer   ;;;;;;;;;;;;;;



timer:
    push ax
    inc word [cs:tickcount] ; increment tick count
    cmp word [cs:tickcount], 18
    jnz l22
l11:
    dec word [cs:sec1]
    mov word[cs:tickcount],0
    cmp word [cs:sec1], 0
    jnz l22 ; Jump to end if seconds reach zero
l33:
    mov word [cs:gameover],1
    jmp endgame
    
l22:
	cmp word [cs:sec1]:1000
	ja endtimer
    push word [cs:sec1]
    call printnum ; Print remaining seconds
endtimer:
    mov al, 0x20
    out 0x20, al ; End of interrupt
    pop ax
    iret ; Return from interrupt



;;;;;;;;;;;;  function to generate random number    ;;;;;;;;;;;;;;



randG:
   push bp
   mov bp, sp
   pusha
   cmp word [rand], 0
   jne next

  MOV     AH, 00h   ; interrupt to get system timer in CX:DX 
  INT     1AH
  inc word [rand]
  mov     [randnum], dx
  jmp next1

  next:
  mov     ax, 25173          ; LCG Multiplier
  mul     word  [randnum]     ; DX:AX = LCG multiplier * seed
  add     ax, 13849          ; Add LCG increment value
  ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
  mov     [randnum], ax          ; Update seed = return value

 next1:xor dx, dx
 mov ax, [randnum]
 mov cx, [bp+4]
 inc cx
 div cx
 
 mov [bp+6], dx
 popa
 pop bp
 ret 2


sound:

     push ax
      push bx
      push cx
      push dx


      mov     al, 182         ; Prepare the speaker for the
      out     43h, al         ;  note.
      mov     ax, 9121        ; Frequency number (in decimal)
      ;  for middle C.
      out     42h, al         ; Output low byte.
      mov     al, ah          ; Output high byte.
      out     42h, al
      in      al, 61h         ; Turn on note (get value from
      ;  port 61h).
      or      al, 00000011b   ; Set bits 1 and 0.
      out     61h, al         ; Send new value.
      mov     bx, 25          ; Pause for duration of note.
      .pause1:
     mov     cx, 6535
      .pause2:
     dec     cx
      jne     .pause2
      dec     bx
      jne     .pause1
      in      al, 61h         ; Turn off note (get value from
      ;  port 61h).
      and     al, 11111100b   ; Reset bits 1 and 0.
      out     61h, al         ; Send new value.

      pop dx
      pop cx
      pop bx
      pop ax

      ret

;;;;;;;;;;;;  function to print welcome screeen    ;;;;;;;;;;;;;;




WelcomeBackGround:
              push es
			  push ax
			  push cx
			  push di
			  push si
			  push dx
 
	
			  mov ax, 0xb800
			  mov es, ax
			  mov ax, 7
			  mov cx,1920
			  cld
			  rep stosw    

				mov ax, 0x9efe;design on upper line
				mov di, 640;leave some space above
				mov cx, 80;run for only 1 line
				cld
				rep stosw;welcome line 1
				
				add di, 174
				mov ah, 0x0f
				mov al, 'W'
				mov word[es:di], ax
				add di, 22
				mov al, 'E'
				mov word[es:di], ax
				add di, 22
				mov al, 'L'
				mov word[es:di], ax
				add di, 22
				mov al, 'C'
				mov word[es:di], ax
				add di, 22
				mov al, 'O'
				mov word[es:di], ax
				add di, 22
				mov al, 'M'
				mov word[es:di], ax
				add di, 22
				mov al, 'E'
				mov word[es:di], ax
				add di, 174

				mov ax, 0x9efe;design on upper line
				mov cx, 80;run for only 1 line
				cld
				rep stosw;welcome line 2

	 
				
          pop dx
          pop si
          pop di
          pop cx
          pop ax
          pop es
          ret



;;;;;;;;;;;;  function to clear screen ;;;;;;;;;;;;;;



clrscr:
    push es
    push ax
    push di
    mov ax,0xb800
    mov es,ax
    mov di,0
    nextloc:
    mov word [es:di], 0x0720
    add di,2
    cmp di,4000
    jne nextloc
    pop di
    pop ax
    pop es 
    ret



;;;;;;;;;;;;  function to delay between things  ;;;;;;;;;;;;;;




delay:
	push bx
	push cx
	push dx
	
	mov bx, ax
	
outerLoop:
	mov cx, bx

innerLoop:
	dec cx
	jnz innerLoop
	
	dec bx
	jnz outerLoop
	
	pop dx
	pop cx
	pop bx
	ret




;;;;;;;;;;;;  function to make screen white   ;;;;;;;;;;;;;;



clrwhite:
    push es
    push ax
    push di
    mov ax,0xb800
    mov es,ax
    mov di,160
    nextloc1:
    mov word [es:di], 0x7000
    add di,2
    cmp di,3840
    jne nextloc1
    pop di
    pop ax
    pop es 
    ret



;;;;;;;;;;;;  function to print boundary   ;;;;;;;;;;;;;;



boundary:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    mov ax,0xb800
    mov es,ax
    mov di,[bp+6]
    mov ax,[bp+4]
    mov bx,[bp+8]
nextloc2:
    mov word [es:di], 0x0720
    add di,ax
    cmp di,bx
    jne nextloc2
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    ret 6

DeleteRowcheck:				
	push bp 
	mov bp,sp		; 
	push ax 
	push bx 
	push cx 
	push dx 
	push es 
	push di 
	push si
	push ds
	mov di,[bp+4]
	mov si, di
	sub si,160
	mov ax, 0xb800
	mov es,ax
	mov ds,ax
	mov dx ,21
	DeleteRowcheck1:
		mov ax , si
		push si
		mov cx ,48
		cld  
		rep movsw
		pop di
		mov si,di
		sub si, 160
		
		dec dx 
		cmp si,160
		jna DeleteRowcheckend
	jnz DeleteRowcheck1
DeleteRowcheckend:

	mov word[scoreflag],1
    pop ds
	 
	mov ax,[cs:scroeeee]
	add ax,10
	mov word[cs:scroeeee],ax
	
	pop si 
	pop di
	pop es 
	pop dx
	pop cx
	pop bx 
	pop ax
	pop bp
    ret 2

;;;;;;;;;;;;  function to print strings  ;;;;;;;;;;;;;;



printstr:
    push bp
    mov bp,sp
    push es
    push ax
    push cx
    push si
    push di
    mov ax,0xb800
    mov es,ax
    mov al,80
    mul byte [bp+10]
    add ax,[bp+12]
    shl ax,1
    mov di,ax
    mov si,[bp+6]
    mov cx,[bp+4]
    mov ah,[bp+8]
nextchar:
    mov al,[si]
    mov [es:di],ax
    add di,2
    add si,1
    loop nextchar
    pop di
    pop si
    pop cx
    pop ax
    pop es
    pop bp
    ret 8



;;;;;;;;;;;;  function to remove row that is completed   ;;;;;;;;;;;;;;



checkrow:
    push bp
    mov bp,sp
    push ax
    push cx
    push bx
    push di
    push si
    push es
    push ds
    push dx
    mov ax,0xb800
    mov es,ax
    mov ds,ax
    mov dx,23
    mov di,3680
    mov ax,3680
    mov bx,3776
checkrowloop1:
    cmp word[es:di],0x7000
    je loop1end
    add di,2
    cmp di,bx
    jne checkrowloop1
    jmp whiterow
loop1end:
    sub ax,160
    mov di,ax
    sub bx,160
    sub dx,1
    jnz checkrowloop1
    jmp rowcheckend
whiterow:
    mov di,ax
	;push ax
	push di
loopwhitecheck1:
    mov word[es:di],0x7000
    add di,2
    cmp di,bx
    jne loopwhitecheck1
	
	pop ax
	push ax
	
	call DeleteRowcheck 
	sub ax,160
    mov di,ax
    sub bx,160
    sub dx,1
    jnz checkrowloop1
    jz rowcheckend
rowcheckend:
     
     pop dx   
     pop ds
     pop es
     pop si
     pop di
     pop bx
     pop cx
     pop ax
     pop bp
     ret




;;;;;;;;;;;;  function to print L shape  ;;;;;;;;;;;;;;



Lshape:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push cx
    mov ax,0xb800
    mov es,ax
    mov cx,[bp+10]
    mov di,[bp+10]
    mov ax,[bp+4]
    mov bx,[bp+8]
loop4:
    mov word [es:di], 0x442A 
    add di,2
    cmp di,ax
    jz end3
    cmp di,bx
    jnz loop4
    add bx,160
    add cx,160
    mov di,cx
    jmp loop4
end3:
    mov ax,3
    mov di,[bp+6]
l1:
    mov word [es:di], 0x442A 
    sub di,2
    sub ax,1
    jnz l1
    pop cx
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    ret 8
vericalLshapecall:
    mov ax,378   ;top left corner
    push ax
    mov word[x1],378
    mov ax,382  ;top right corner
    push ax
    mov word[x2],382  
    mov ax,698  ;bottom left corner
    push ax
    mov word[x3],698
    mov ax,702  ;bottom right corner
    push ax    
    mov word[x4],702
    call Lshape
    ret



;;;;;;;;;;;;  function to I shape     ;;;;;;;;;;;;;;



Ishape:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push cx
    mov ax,0xb800
    mov es,ax
    mov cx,[bp+10]
    mov di,[bp+10]
    mov ax,[bp+4]
    mov bx,[bp+8]
loop3:
    mov word [es:di], 0x222A 
    add di,2
    cmp di,ax
    jz end2
    cmp di,bx
    jnz loop3
    add bx,160
    add cx,160
    mov di,cx
    jmp loop3
end2:
    pop cx
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    ret 8

Ishapecall:
    mov ax,378   ;top left corner
    push ax
    mov word[x1],378
    mov ax,382  ;top right corner
    push ax
    mov word[x2],382  
    mov ax,698  ;bottom left corner
    push ax
    mov word[x3],698
    mov ax,702  ;bottom right corner
    push ax    
    mov word[x4],702
    call Ishape
    ret





;;;;;;;;;;;;  function to print square shape  ;;;;;;;;;;;;;;



square:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push cx
    mov ax,0xb800
    mov es,ax
    mov cx,[bp+10]
    mov di,[bp+10]
    mov ax,[bp+4]
    mov bx,[bp+8]
loop1:
    mov word [es:di], 0x5520
    add di,2
    cmp di,ax
    jz end
    cmp di,bx
    jnz loop1
    add bx,160
    add cx,160
    mov di,cx
    jmp loop1
end:
    pop cx
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    ret 8

squarecall:
    mov ax,370  ;top left corner
    push ax
    mov word[x1],370
    mov ax,382   ;top right corner
    push ax
    mov word[x2],382   
    mov ax,690   ;bottom left corner
    push ax
    mov word[x3],690
    mov ax,702   ;bottom right corner
    push ax
    mov word[x4],702
    call square
    ret




;;;;;;;;;;;;  function to print horizontal L   ;;;;;;;;;;;;;;




horizontalL:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    mov ax,0xb800
    mov es,ax
    mov di,[bp+6]
    mov ax,[bp+4]
    mov bx,[bp+8]
nextloc4:
    mov word [es:di], 0x1120
    add di,ax
    cmp di,bx
    jne nextloc4
    
    mov cx,2
    mov di,bx
    sub di,2
l2:

    mov word [es:di], 0x1120
    add di,160
    sub cx,1
    jnz l2
    mov cx,2
    sub di,160
    sub di,2
l3:
    mov word [es:di], 0x1120
    sub di,160
    sub cx,2
    jnz l3
    
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    ret 6
horizontalLshapecall:
    mov ax,542 ;ending address
    push ax
    mov word[x2],542
    mov ax,530 ; starting address
    push ax
    mov word[x1],530
    mov ax,2 ;adding after each ietration
    push ax
    mov word[x3],2
    call horizontalL
    ret



;;;;;;;;;;;;  function to print horizontal I     ;;;;;;;;;;;;;;



horizontalI:
	push bp
	mov bp,sp
	push es
	push bx
	push ax
	push di
	mov ax,0xb800
	mov es,ax
	mov di,[bp+6]
	mov ax,[bp+4] 
	mov bx,[bp+8]
nextloc5:
  mov word [es:di], 6600h
  add di, ax
  cmp di, bx
  jnz nextloc5

	pop di
	pop ax
	pop bx
	pop es 
	pop bp
	ret 6
horizontalIshapecall:
	mov ax,542 ;ending address
	push ax
    	mov word[x2],542
	mov ax,530 ; starting address
	push ax
    	mov word[x1],530
	mov ax,2 ;adding after each ietration
	push ax
	mov word[x3],2
	call horizontalI
	ret



;;;;;;;;;;;;  function to print white square at next shape block    ;;;;;;;;;;;;;;



whitesquare:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push cx
    mov ax,0xb800
    mov es,ax
    mov cx,[bp+10]
    mov di,[bp+10]
    mov ax,[bp+4]
    mov bx,[bp+8]
loopwhite1:
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz endwhite
    cmp di,bx
    jnz loopwhite1
    add bx,160
    add cx,160
    mov di,cx
    jmp loopwhite1
endwhite:
    pop cx
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    ret 8

squarewhitecall:
     mov ax,2830   ;top left corner
    push ax
    mov ax,2852 ;top right corner
    push ax
    mov ax,3470  ;bottom left corner
    push ax
    mov ax,3492  ;bottom right corner
    push ax
    call whitesquare
    ret





;;;;;;;;;;;;  function to print shape at next block     ;;;;;;;;;;;;;;



nextshape:

	push bp
 	mov bp,sp
	push ax
	mov ax,[bp+4]  
	cmp ax,0
        jz ShapeI1
	cmp ax,1
        jz squarecall1
	cmp ax,2
        jz horizontalIshapecall1
	cmp ax,3
        jz horizontalLshapecall1
	cmp ax,4
        jz vericalLshapecall1

vericalLshapecall1:
    mov ax,2838   ;top left corner
    push ax
    mov ax,2842 ;top right corner
    push ax
    mov ax,3158  ;bottom left corner
    push ax
    mov ax,3162  ;bottom right corner
    push ax
    call Lshape
    jmp end1

horizontalLshapecall1:
 	mov ax,2850 ;ending address
 	push ax
        mov ax,2838 ; starting address
        push ax
	mov ax,2 ;adding after each ietration
	push ax
	call horizontalL
	jmp end1
horizontalIshapecall1:
	mov ax,2850 ;ending address
	push ax
	mov ax,2838 ; starting address
	push ax
	mov ax,2 ;adding after each ietration
	push ax
	call horizontalI
	jmp end1

squarecall1:
    mov ax,2838  ;top left corner
    push ax
    mov ax,2850   ;top right corner
    push ax
    mov ax,3158   ;bottom left corner
    push ax
    mov ax,3170   ;bottom right corner
    push ax
    call square
    jmp end1


ShapeI1:
    mov ax,2838   ;top left corner 
    push ax
    mov ax,2842 ;top right corner
    push ax
    mov ax,3158  ;bottom left corner
    push ax
    mov ax,3162  ;bottom right corner
    push ax
    call Ishape
    jmp end1
end1:
pop ax
pop bp
ret



;;;;;;;;;;;;  function for right movement   ;;;;;;;;;;;;;;




rightmovement:
	push bp
	mov bp,sp
	push dx
	mov dx,[bp+4]
	cmp dx,0
	jz Iright1
	cmp dx,1
	jz squareright1
	cmp dx,2
	jz HorizontalIright1
	cmp dx,3
	jz horizontalLright1
	cmp dx,4
	jz verticalLshaperight1
Iright1:
   jmp Iright
squareright1:
   jmp squareright
HorizontalIright1:
   jmp HorizontalIright
horizontalLright1:
   jmp horizontalLright
verticalLshaperight1:
   jmp verticalLshaperight

Iright:
    push cx
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push si
    mov ax,0xb800
    mov es,ax
Ipp1:
    ;add word[x1],6
    ;add word[x3],6
    mov di,[x2]
    mov ax,[x4]
    
Ip1:    
    cmp word [es:di], 0x7000
    jne endd2
    add di,160
    cmp di,ax
    jz Ip2
    jnz Ip1
endd2:
jmp end33
Ip2: 
    ;sub word[x1],6
    ;sub word[x3],6
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]  
    
I1:   
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz I2
    cmp di,bx
    jnz I1
    add bx,160
    add cx,160
    mov di,cx
    jmp I1
I2:
    add word[x1],2
    add word[x2],2
    add word[x3],2
    add word[x4],2
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov word[x1],di
    mov word[x2],bx
    mov word[x4],ax
I3:
    mov word [es:di], 0x222A 
    add di,2
    cmp di,ax
    jz Iendright4
    cmp di,bx
    jnz I3
    add bx,160
    add cx,160
    mov di,cx
    jmp I3
    
end33:
    ;add word[x1],6
    ;add word[x3],6
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov [x1],di
    mov [x4],ax
    mov [x2],bx
Iendright4:
    mov word[print],0 
    pop cx
    pop si
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp rightmovementend


squareright:
    push cx
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push si
    mov ax,0xb800
    mov es,ax
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
squarep:
    add word[x1],14
    add word[x3],14
    mov di,[x2]
    mov ax,[x4]
Ipsquare1:    
    cmp word [es:di], 0x7000
    jne endsquare2
    add di,160
    cmp di,ax
    jz Ipsquare2
    jmp Ipsquare1
endsquare2:
jmp endsquare3
Ipsquare2:
    sub word[x1],14
    sub word[x3],14
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]


square1:
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz square2
    cmp di,bx
    jnz square1
    add bx,160
    add cx,160
    mov di,cx
    jmp square1
square2:
    add word[x1],2
    add word[x2],2
    add word[x3],2
    add word[x4],2
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
square3:
    mov word [es:di], 0x5520
    add di,2
    cmp di,ax
    jz squareend
    cmp di,bx
    jnz square3
    add bx,160
    add cx,160
    mov di,cx
    jmp square3
endsquare3:
    sub word[x1],14
    sub word[x3],14
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
squareend:
    pop cx
    pop si
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp rightmovementend


HorizontalIright:
        push bp
	mov bp,sp
	push es
	push bx
	push ax
	push di
	mov ax,0xb800
	mov es,ax
	mov di,[x1]
	mov ax,[x3]
	mov bx,[x2]
HorizontalIright11p:
	add word[x1],14
	mov di,[x2]
HorizontalIright1p:    
    cmp word [es:di], 0x7000
    jne endHorizontalIright2
    jmp HorizontalIright2p
endHorizontalIright2:
jmp endHorizontalIright3
HorizontalIright2p:
    sub word[x1],14
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]

HorizontalIright11:
  	mov word [es:di], 0x7000
 	add di, ax
	cmp di, bx
 	jnz HorizontalIright11

	add word[x1],2
	add word[x2],2
	mov di,[x1]
	mov ax,[x3]
	mov bx,[x2]
HorizontalIright2h:
  mov word [es:di], 6600h
  add di, ax
  cmp di, bx
  jnz HorizontalIright2h
  jmp HorizontalIrightend
endHorizontalIright3:
    sub word[x1],14
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]

HorizontalIrightend:
	
	pop di
	pop ax
	pop bx
	pop es 
	pop bp
	jmp rightmovementend



horizontalLright:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    mov ax,0xb800
    mov es,ax
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
Ihorizontalrightp1:
    add word[x1],14
    mov di,[x2]
horizontalL111rightp:
    cmp word [es:di], 0x7000
    jne endlhorizontalLright2
    jmp IphorizontalLright2

endlhorizontalLright2:
jmp endhorizontalLright3

IphorizontalLright2:
    sub word[x1],14
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]

horizontalLright111:
    mov word [es:di], 0x7000
    add di,ax
    cmp di,bx
    jne horizontalLright111

   mov cx,2
   mov di,bx
    sub di,2
horizontalLright2:
    mov word [es:di], 0x7000
    add di,160
    sub cx,1
    jnz horizontalLright2

    mov cx,2
    sub di,160
    sub di,2
horizontalLright3:
    mov word [es:di], 0x7000
    sub di,160
    sub cx,2
    jnz horizontalLright3


    add word[x2],2
    add word[x1],2
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
    mov [x1],di
    mov [x2],bx
    mov [x3],ax
    
horizontalLright4:
    mov word [es:di], 0x1120
    add di,ax
    cmp di,bx
    jne horizontalLright4

    mov cx,2
   
    mov di,bx
    sub di,2
horizontalLright5:
    mov word [es:di], 0x1120
    add di,160
    sub cx,1
    jnz horizontalLright5
    mov cx,2
    sub di,160
    sub di,2
horizontalLright6:
    mov word [es:di], 0x1120
    sub di,160
    sub cx,2
    jnz horizontalLright6
    jmp endhorizontalLright4
endhorizontalLright3:
    sub word[x1],14
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
    mov [x1],di
    mov [x3],ax
    mov [x2],bx
endhorizontalLright4:
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp rightmovementend



verticalLshaperight:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push cx
    mov ax,0xb800
    mov es,ax
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
verticalLshaperightp:
    ;add word[x1],6
    ;add word[x3],6
    mov di,[x2]
    mov ax,[x4]
    
IpverticalLshaperight111:
    cmp word [es:di], 0x7000
    jne endverticalLshaperight0
    add di,160
    cmp di,ax
    jnz IpverticalLshaperight111
    jmp IpverticalLshaperight2

endverticalLshaperight0:    
jmp endverticalLshaperight3
IpverticalLshaperight2:
    ;sub word[x1],6
    ;sub word[x3],6
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]



verticalLshaperight111:
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz verticalLshaperightend
    cmp di,bx
    jnz verticalLshaperight111
    add bx,160
    add cx,160
    mov di,cx
    jmp verticalLshaperight111
verticalLshaperightend:
    mov ax,3
    mov di,[x3]
verticalLshaperight20:
    mov word [es:di], 0x7000
    sub di,2
    sub ax,1
    jnz verticalLshaperight20
    
    add word[x1],2
    add word[x4],2
    add word[x2],2
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov word[x1],di
    mov word[x2],bx
    mov word[x4],ax
    verticalLshaperight3:
    mov word [es:di], 0x442A
    add di,2
    cmp di,ax
    jz verticalLshaperightend1
    cmp di,bx
    jnz verticalLshaperight3
    add bx,160
    add cx,160
    mov di,cx
    jmp verticalLshaperight3
verticalLshaperightend1:
    mov ax,3
    add word[x3],2
    mov di,[x3]
verticalLshaperight4:
    mov word [es:di], 0x442A
    sub di,2
    sub ax,1
    jnz verticalLshaperight4
    jmp verticalLshaperightend10
endverticalLshaperight3:
    ;sub word[x1],6
    ;sub word[x3],6
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]


verticalLshaperightend10:

    pop cx
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
   jmp rightmovementend
rightmovementend: 
   pop dx 
   pop bx
   ret 2



;;;;;;;;;;;;  function for left movement    ;;;;;;;;;;;;;;




leftmovement:
	push bp
	mov bp,sp
	push dx
	mov dx,[bp+4]
	cmp dx,0
	jz Ileft10
	cmp dx,1
	jz squareleft10
	cmp dx,2
	jz HorizontalIleft1
	cmp dx,3
	jz horizontalLleft1
	cmp dx,4
	jz verticalLshapeleft1
Ileft10:
   jmp Ileft
squareleft10:
   jmp squareleft
HorizontalIleft1:
   jmp HorizontalIleft
horizontalLleft1:
   jmp horizontalLleft
verticalLshapeleft1:
   jmp verticalLshapeleft


HorizontalIleft:
        push bp
	mov bp,sp
	push es
	push bx
	push ax
	push di
	mov ax,0xb800
	mov es,ax
	mov di,[x1]
	mov ax,[x3]
	mov bx,[x2]
HorizontalIleft11p:
	sub word[x1],2
	mov di,[x1]
HorizontalIleft1p:    
    cmp word [es:di], 0x7000
    jne endHorizontalIleft2
    je HorizontalIleft2p

endHorizontalIleft2:
jmp endHorizontalIleft3

HorizontalIleft2p:
    add word[x1],2
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]


HorizontalIleft11:
  	mov word [es:di], 0x7000
 	add di, ax
	cmp di, bx
 	jnz HorizontalIleft11

        sub word[x1],2
	sub word[x2],2
	mov di,[x1]
	mov bx,[x2]
	mov ax,[x3]
HorizontalIleft2:
  mov word [es:di], 6600h
  add di, ax
  cmp di, bx
  jnz HorizontalIleft2
	
  jmp HorizontalIleftend
endHorizontalIleft3:
    add word[x1],2
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
HorizontalIleftend:
	
	pop di
	pop ax
	pop bx
	pop es 
	pop bp
	jmp leftmovementend

squareleft:
    push cx
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push si
    mov ax,0xb800
    mov es,ax
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
squarepleft:
    sub word[x2],14
    sub word[x4],14
    mov di,[x2]
    mov ax,[x4]
Ipsquareleft1:    
    cmp word [es:di], 0x7000
    jne endsquareleft2
    add di,160
    cmp di,ax
    jz Ipsquareleft2
    jmp Ipsquareleft1
endsquareleft2:
jmp endsquareleft3
Ipsquareleft2:
    add word[x2],14
    add word[x4],14
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]

squareleft1:
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz squareleft2
    cmp di,bx
    jnz squareleft1
    add bx,160
    add cx,160
    mov di,cx
    jmp squareleft1
squareleft2:
    sub word[x1],2
    sub word[x2],2
    sub word[x3],2
    sub word[x4],2
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
squareleft3:
    mov word [es:di], 0x5520
    add di,2
    cmp di,ax
    jz squareleftend
    cmp di,bx
    jnz squareleft3
    add bx,160
    add cx,160
    mov di,cx
    jmp squareleft3
endsquareleft3:
    add word[x2],14
    add word[x4],14
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]


squareleftend:
    pop cx
    pop si
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp leftmovementend



Ileft:
    push cx
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push si
    mov ax,0xb800
    mov es,ax
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
Ileftp1:
    sub word[x2],6
    sub word[x4],6
    mov di,[x2]
    mov ax,[x4]
Ileft1:    
    cmp word [es:di], 0x7000
    jne endleft2
    add di,160
    cmp di,ax
    jz Ipleft2
    jnz Ileft1
endleft2:
jmp endleft3
Ipleft2:
    add word[x2],6
    add word[x4],6
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]  
    

Ileftpp1:    
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz Ileft2
    cmp di,bx
    jnz Ileftpp1
    add bx,160
    add cx,160
    mov di,cx
    jmp Ileftpp1
Ileft2:
    sub word[x1],2
    sub word[x2],2
    sub word[x3],2
    sub word[x4],2
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov word[x1],di
    mov word[x2],bx
    mov word[x4],ax
Ileft3:
    mov word [es:di], 0x222A 
    add di,2
    cmp di,ax
    jz Iendleft4
    cmp di,bx
    jnz Ileft3
    add bx,160
    add cx,160
    mov di,cx
    jmp Ileft3
endleft3:
    add word[x2],6
    add word[x4],6
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov [x1],di
    mov [x4],ax
    mov [x2],bx
Iendleft4:
    mov word[print],0 
    pop cx
    pop si
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    
    jmp leftmovementend


horizontalLleft:
     push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    mov ax,0xb800
    mov es,ax
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
Ihorizontalp1:
    sub word[x1],2
    mov di,[x1]
horizontalLleft111p:
    cmp word [es:di], 0x7000
    jne endlhorizontalL2
    add di,160
    mov cx,1
    add di,8
horizontalLleft2p:
    cmp word [es:di], 0x7000
    jne endlhorizontalL2
    add di,2
    sub cx,1
    cmp cx,0
    jnz horizontalLleft2p
    jz IphorizontalL2
endlhorizontalL2:
jmp endhorizontalL3

IphorizontalL2:
    add word[x1],2
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
    

horizontalLleft111:
    mov word [es:di], 0x7000
    add di,ax
    cmp di,bx
    jne horizontalLleft111

    mov cx,2
    mov di,bx
    sub di,2
horizontalLleft2:
    mov word [es:di], 0x7000
    add di,160
    sub cx,1
    cmp cx,0
    jnz horizontalLleft2

    mov cx,2
    sub di,160
    sub di,2
horizontalLleft3:
    mov word [es:di], 0x7000
    sub di,160
    sub cx,2
    jnz horizontalLleft3
    
    sub word[x2],2
    sub word[x1],2
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
    mov [x1],di
    mov [x2],bx
    mov [x3],ax
    
   
horizontalLleft4:
    mov word [es:di], 0x1120
    add di,ax
    cmp di,bx
    jne horizontalLleft4

    
    mov cx,2
    mov di,bx
    sub di,2
horizontalLleft5:
    mov word [es:di], 0x1120
    add di,160
    sub cx,1
    jnz horizontalLleft5
    mov cx,2
    sub di,160
    sub di,2
horizontalLleft6:
    mov word [es:di], 0x1120
    sub di,160
    sub cx,2
    jnz horizontalLleft6
    jmp endhorizontalL4
endhorizontalL3:
    add word[x1],2
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
    mov [x1],di
    mov [x3],ax
    mov [x2],bx
endhorizontalL4:
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp leftmovementend


verticalLshapeleft:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push cx
    mov ax,0xb800
    mov es,ax
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
verticalLshapeleftp:
    mov di,[x1]
    sub di,6
    mov cx,2
IpverticalLshapeleft1110:
    cmp word [es:di], 0x7000
    jne endverticalLshapeleft0
    add di,160
    sub cx,1
    jnz IpverticalLshapeleft1110

    mov di,[x4]
IpverticalLshapeleft111:
    cmp word [es:di], 0x7000
    jne endverticalLshapeleft0
    jmp IpverticalLshapeleft2
endverticalLshapeleft0:    
jmp endverticalLshapeleft3
IpverticalLshapeleft2:
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]



verticalLshapeleft111:
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz verticalLshapeleftend
    cmp di,bx
    jnz verticalLshapeleft111
    add bx,160
    add cx,160
    mov di,cx
    jmp verticalLshapeleft111
verticalLshapeleftend:
    mov ax,3
    ;add word[x3],8
    mov di,[x3]
verticalLshapeleft20:
    mov word [es:di], 0x7000
    sub di,2
    sub ax,1
    jnz verticalLshapeleft20
    
    sub word[x1],2
    sub word[x4],2
    sub word[x2],2
    sub word[x3],2
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov word[x1],di
    mov word[x2],bx
    mov word[x4],ax

verticalLshapeleft3:
    mov word [es:di], 0x442A
    add di,2
    cmp di,ax
    jz verticalLshapeleftend1
    cmp di,bx
    jnz verticalLshapeleft3
    add bx,160
    add cx,160
    mov di,cx
    jmp verticalLshapeleft3
verticalLshapeleftend1:
    mov ax,3
    ;add word[x3],4
    mov di,[x3]
verticalLshapeleft4:
    mov word [es:di], 0x442A
    sub di,2
    sub ax,1
    jnz verticalLshapeleft4
    jmp verticalLshapeleftend10
endverticalLshapeleft3:
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]


verticalLshapeleftend10:

    pop cx
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp leftmovementend

leftmovementend: 
   pop dx 
   pop bx
   ret 2




;;;;;;;;;;;;  function for down movement;;;;;;;;;;;;;;





downmovement:
	push bp
	mov bp,sp
	push dx
	mov dx,[bp+4]
	cmp dx,0
	jz Idown10
	cmp dx,1
	jz squaredown10
	cmp dx,2
	jz HorizontalIdown1
	cmp dx,3
	jz horizontalLdown1
	cmp dx,4
	jz verticalLshapedown1
Idown10:
   jmp Idown
squaredown10:
   jmp squaredown
HorizontalIdown1:
   jmp HorizontalIdown
horizontalLdown1:
   jmp horizontalLdown
verticalLshapedown1:
   jmp verticalLshapedown



HorizontalIdown:
        push bp
	mov bp,sp
	push es
	push bx
	push ax
	push di
	mov ax,0xb800
	mov es,ax
	mov di,[x1]
	mov ax,[x3]
	mov bx,[x2]

HorizontalIdown11p:
	add word[x1],160
	add word[x2],160
	mov di,[x1]
	mov ax,[x3]
	mov bx,[x2]
HorizontalIdown1p:    
    cmp word [es:di], 0x7000
    jne endHorizontalIdown2
    add di, ax
    cmp di, bx
    jz HorizontalIdown2p
    cmp di,bx
    jnz HorizontalIdown1p
    add bx,160
    add cx,160
    mov di,cx
    jmp HorizontalIdown1p

endHorizontalIdown2:
jmp endHorizontalIdown3

HorizontalIdown2p:
    sub word[x1],160
    sub word[x2],160
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]

HorizontalIdown11:
  	mov word [es:di], 0x7000
 	add di, ax
	cmp di, bx
 	jnz HorizontalIdown11
        
        add word[x1],160
 	add word[x2],160
	mov di,[x1]
	mov bx,[x2]
	mov ax,[x3]
	mov word[x1],di
	mov word[x2],bx
	mov word[x3],ax
	
HorizontalIdown2:
  mov word [es:di], 6600h
  add di, ax
  cmp di, bx
  jnz HorizontalIdown2
   jmp HorizontalIdownend
endHorizontalIdown3:
    mov word[move],0
    call checkrow
	call checkrow
	; cmp word[scoreflag],1
	; jne endHorizontalIdown31
	; mov ax,0xb800
    ; mov es,ax
    ; mov di, 664
	; rowcheckend1:
	; mov word[es:di],0x7000
	; add di,2
	; cmp di, 680
	; jne rowcheckend1
; endHorizontalIdown31:	
    sub word[x1],160
    sub word[x2],160
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
	
HorizontalIdownend:
	
	pop di
	pop ax
	pop bx
	pop es 
	pop bp
	jmp downmovementend

squaredown:
    push cx
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push si
    mov ax,0xb800
    mov es,ax
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
squarepdown:
    add word[x1],480
    add word[x2],480
    mov di,[x1]
    mov ax,[x2]
Ipsquaredown1:    
    cmp word [es:di], 0x7000
    jne endsquaredown2
    add di,2
    cmp di,ax
    jz Ipsquaredown2
    jmp Ipsquaredown1
endsquaredown2:
jmp endsquaredown3
Ipsquaredown2:
    sub word[x1],480
    sub word[x2],480
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]


squaredown1:
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz squaredown2
    cmp di,bx
    jnz squaredown1
    add bx,160
    add cx,160
    mov di,cx
    jmp squaredown1
squaredown2:
    add word[x1],160
    add word[x2],160
    add word[x4],160
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
squaredown3:
    mov word [es:di], 0x5520
    add di,2
    cmp di,ax
    jz squaredownend
    cmp di,bx
    jnz squaredown3
    add bx,160
    add cx,160
    mov di,cx
    jmp squaredown3
endsquaredown3:
    mov word[move],0
    call checkrow
	call checkrow
	; cmp word[scoreflag],1
	; jne endsquaredown31
	; mov ax,0xb800
    ; mov es,ax
    ; mov di, 664
	; rowcheckend2:
	; mov word[es:di],0x7000
	; add di,2
	; cmp di, 680
	; jne rowcheckend2
; endsquaredown31:
    sub word[x1],480
    sub word[x2],480
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]


squaredownend:
    
    pop cx
    pop si
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp downmovementend



Idown:
    push cx
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push si
    mov ax,0xb800
    mov es,ax
    mov cx,[x1]
    mov si,cx
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
Idownp1:
    add word[x1],480
    add word[x2],480
    mov di,[x1]
    mov ax,[x2]
Ipdown1:    
    cmp word [es:di], 0x7000
    jne enddown2
    add di,2
    cmp di,ax
    jz Ipdown2
    jnz Ipdown1

enddown2:
jmp enddown3
Ipdown2: 
    sub word[x1],480
    sub word[x2],480
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    
Idown1:    
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz Idown2
    cmp di,bx
    jnz Idown1
    add bx,160
    add cx,160
    mov di,cx
    jmp Idown1
Idown2:
    add word[x1],160
    add word[x2],160
    add word[x4],160
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov si,2
Idown3:
    mov word [es:di], 0x222A 
    add di,2
    cmp di,ax
    jz enddown4
    cmp di,bx
    jnz Idown3
    add bx,160
    add cx,160
    mov di,cx
    jmp Idown3

enddown3:
    mov word[move],0
    call checkrow
	call checkrow
	; cmp word[scoreflag],1
	; jne endIdown31
	; mov ax,0xb800
    ; mov es,ax
    ; mov di, 664
	; rowcheckend3:
	; mov word[es:di],0x7000
	; add di,2
	; cmp di, 680
	; jne rowcheckend3
; endIdown31:
    sub word[x1],480
    sub word[x2],480
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov [x1],di
    mov [x4],ax
    mov [x2],bx
enddown4:
    mov word[print],0 
    
    pop cx
    pop si
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp downmovementend


horizontalLdown:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    mov ax,0xb800
    mov es,ax
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
Ihorizontaldownp1:
    add word[x1],160
    add word[x2],160
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
    sub bx,4
horizontalL111downp0:
    cmp word [es:di], 0x7000
    jne endlhorizontalLdown2
    add di,ax
    cmp di,bx
    jne horizontalL111downp0
    add di,160
    add di,2
    add bx,160
    add bx,4
    ;add di,8
horizontalL111downp:
    cmp word [es:di], 0x7000
    jne endlhorizontalLdown2
    add di,ax
    cmp di,bx
    jne horizontalL111downp
    jmp IphorizontalLdown2

endlhorizontalLdown2:
jmp endhorizontalLdown3
IphorizontalLdown2:
    sub word[x1],160
    sub word[x2],160
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]

horizontalLdown111:
    mov word [es:di], 0x7000
    add di,ax
    cmp di,bx
    jne horizontalLdown111

    mov cx,2
    mov di,bx
    sub di,2
horizontalLdown2:
    mov word [es:di], 0x7000
    add di,160
    sub cx,1
    cmp cx,0
    jnz horizontalLdown2

    mov cx,2
    sub di,160
    sub di,2
horizontalLdown3:
    mov word [es:di], 0x7000
    sub di,160
    sub cx,2
    jnz horizontalLdown3
    
    add word[x1],160
    add word[x2],160
    mov bx,[x2]
    mov ax,[x3]
    mov di,[x1]
    mov [x1],di
    mov [x2],bx
    mov [x3],ax
    
   
horizontalLdown4:
    mov word [es:di], 0x1120
    add di,ax
    cmp di,bx
    jne horizontalLdown4

    
    mov cx,2
    mov di,bx
    sub di,2
horizontalLdown5:
    mov word [es:di], 0x1120
    add di,160
    sub cx,1
    jnz horizontalLdown5
    mov cx,2
    sub di,160
    sub di,2
horizontalLdown6:
    mov word [es:di], 0x1120
    sub di,160
    sub cx,2
    jnz horizontalLdown6
    jmp endhorizontalLdown4
endhorizontalLdown3:
    mov word[move],0
    call checkrow
	call checkrow
	; cmp word[scoreflag],1
	; jne endhorizontalLdown31
	; mov ax,0xb800
    ; mov es,ax
    ; mov di, 664
	; rowcheckend4:
	; mov word[es:di],0x7000
	; add di,2
	; cmp di, 680
	; jne rowcheckend4
; endhorizontalLdown31:
    sub word[x2],160
    sub word[x1],160
    mov di,[x1]
    mov ax,[x3]
    mov bx,[x2]
    mov [x1],di
    mov [x3],ax
    mov [x2],bx
endhorizontalLdown4:
    
    
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp downmovementend


verticalLshapedown:
    push bp
    mov bp,sp
    push es
    push bx
    push ax
    push di
    push cx
    mov ax,0xb800
    mov es,ax
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
verticalLshapedownp:
    add word[x4],160
    mov di,[x4]
    sub di,2
    mov cx,4
IpverticalLshapedown111:
    cmp word [es:di], 0x7000
    jne endverticalLshapedown0
    sub di,2
    sub cx,1
    cmp cx,0
    jnz IpverticalLshapedown111
    jmp IpverticalLshapedown2
endverticalLshapedown0:    
jmp endverticalLshapedown3
IpverticalLshapedown2:
    sub word[x4],160
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]



verticalLshapedown111:
    mov word [es:di], 0x7000
    add di,2
    cmp di,ax
    jz verticalLshapedownend
    cmp di,bx
    jnz verticalLshapedown111
    add bx,160
    add cx,160
    mov di,cx
    jmp verticalLshapedown111
verticalLshapedownend:
    mov ax,3
    
    mov di,[x3]
verticalLshapedown20:
    mov word [es:di], 0x7000
    sub di,2
    sub ax,1
    jnz verticalLshapedown20
    
    add word[x1],160
    add word[x4],160
    add word[x2],160
    add word[x3],160
    mov cx,[x1]
    mov di,[x1]
    mov ax,[x4]
    mov bx,[x2]
    mov word[x1],di
    mov word[x2],bx
    mov word[x4],ax

verticalLshapedown3:
    mov word [es:di], 0x442A
    add di,2
    cmp di,ax
    jz verticalLshapedownend1
    cmp di,bx
    jnz verticalLshapedown3
    add bx,160
    add cx,160
    mov di,cx
    jmp verticalLshapedown3
verticalLshapedownend1:
    mov ax,3
    ;add word[x3],4
    mov di,[x3]
verticalLshapedown4:
    mov word [es:di], 0x442A
    sub di,2
    sub ax,1
    jnz verticalLshapedown4
    jmp verticalLshapedownend10
endverticalLshapedown3:
    mov word[move],0
    call checkrow
	call checkrow
	; cmp word[scoreflag],1
	; jne endverticalLdown3111
	; mov ax,0xb800
    ; mov es,ax
    ; mov di, 664
	; rowcheckend5:
	; mov word[es:di],0x7000
	; add di,2
	; cmp di, 680
	; jnz rowcheckend5
; endverticalLdown3111:
    sub word[x4],160
    
   


verticalLshapedownend10:
    
    pop cx
    pop di
    pop ax
    pop bx
    pop es 
    pop bp
    jmp downmovementend

downmovementend: 
   
   pop dx 
   pop bx
   ret 2





;;;;;;;;;;;;  start of game    ;;;;;;;;;;;;;;





start:
    


;;;;;;;;;;;;  calling the starting screen    ;;;;;;;;;;;;;;



    call clrscr
    call WelcomeBackGround
    mov ax,22 ;column
    push ax
    mov ax,12  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, message1
    push ax
    push word [lengthm]
    call printstr
    mov ah,0 
    int 16h
        


;;;;;;;;;;;;  calling the sinstruction screen    ;;;;;;;;;;;;;;



	call clrscr
    mov ax,10 ;column
    push ax
    mov ax,2  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, name6
    push ax
    push word [length6]
    call printstr
     
	mov ax,22 ;column
    push ax
    mov ax,3  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, name7
    push ax
    push word [length7]
    call printstr

	mov ax,10 ;column
    push ax
    mov ax,5  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, name8
    push ax
    push word [length8]
    call printstr

	mov ax,22 ;column
    push ax
    mov ax,7  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, name9
    push ax
    push word [length9]
    call printstr
	
	mov ax,22 ;column
    push ax
    mov ax,9  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, name10
    push ax
    push word [length10]
    call printstr
	
	mov ax,22 ;column
    push ax
    mov ax,11  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, name11
    push ax
    push word [length11]
    call printstr
	
	mov ax,22 ;column
    push ax
    mov ax,13  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, name12
    push ax
    push word [length12]
    call printstr
	
	
	mov ax,10 ;column
    push ax
    mov ax,16  ;rows
    push ax
    mov ax,7   ;colour
    push ax
    mov ax, message1
    push ax
    push word [lengthm]
    call printstr
    mov ah,0 
    int 16h
	call clrwhite

	
;;;;;;;;;;;;  hooking the timer    ;;;;;;;;;;;;;;



    xor ax, ax
    mov es, ax ; point es to IVT base 
    mov ax,[es:8*4]
    mov [oldisr],ax
    mov ax,[es:8*4+2]
    mov [oldisr+2],ax
    cli ; disable interrupts
    mov word [es:8*4], timer ; store offset at n*4
    mov [es:8*4+2], cs ; store segment at n*4+2
    sti ; enable interrupts



;;;;;;;;;;;;  printing the board for game     ;;;;;;;;;;;;;;




    mov ax,3998 ;ending address
    push ax
    mov ax,158 ;starting address
    push ax
    mov ax,160 ;adding after each ietration
    push ax
    call boundary

    mov ax,3840 ;ending address
    push ax
    mov ax,0 ;starting address
    push ax
    mov ax,160 ;adding after each ietration
    push ax
    call boundary


    mov ax,3938 ;ending address
    push ax
    mov ax,98 ;starting address
    push ax
    mov ax,160 ;adding after each ietration
    push ax
    call boundary



;;;;;;;;;;;;;;;  total score printing ;;;;;;;;;;;;;;;;;;;




    ;score printing
    push word [scroeeee]
    call printnum1



;;;;;;;;;;;;;;;   score printing ;;;;;;;;;;;;;;;;;;;
    

    ;score printing
    mov ax,60  ;column
    push ax
    mov ax,4  ;row
    push ax
    mov ax,112 ;colour
    push ax
    mov ax, name1
    push ax
    push word [length1]
    call printstr


	
;;;;;;;;;;;;;;;  time printing ;;;;;;;;;;;;;;;;;;;



    ;time printing
    mov ax,60 ;column
    push ax
    mov ax,10  ;rows
    push ax
    mov ax,112   ;colour
    push ax
    mov ax, name2
    push ax
    push word [length2]
    call printstr



;;;;;;;;;;;;;;;  next block printing ;;;;;;;;;;;;;;;;;;;



    ;next block printing
    mov ax,57   ;column
    push ax
    mov ax,15    ;rows
    push ax
    mov ax,112    ;colour
    push ax
    mov ax, name4
    push ax
    push word [length4]
    call printstr
	
        



;;;;;;;;;;;;  loop for continous game play    ;;;;;;;;;;;;;;

			mov word[currentshape],0

	
gamecontinue:	



    ;;;;;;;;;;;;;;;;       score printing  ;;;;;;;;;;;;;;;;

   
    push word [scroeeee]
    call printnum1


	cmp word[currentshape],0
	je s0
	cmp word[currentshape],1
	je s1
	cmp word[currentshape],2
	je s2
	cmp word[currentshape],3
	je s3
	cmp word[currentshape],4
	je s4
s0:
    ;verical line
    call Ishapecall
    jmp nextshape2
s1:
	;square
    call squarecall
    jmp nextshape2
s2:
	;horizontal line
    call horizontalIshapecall
    jmp nextshape2
s3:
    ;horizontalL line
    call horizontalLshapecall
    jmp nextshape2
s4:
     ;vericalL line
    call vericalLshapecall
    jmp nextshape2


;;;;;;;;;;;;  using random function to generate new shape    ;;;;;;;;;;;;;;


nextshape2:
    call squarewhitecall
	sub sp, 2
	push 4
	call randG
	pop dx
    ;mov word[nextshape1],2
	mov word[nextshape1],dx
  	push word[nextshape1]
	call nextshape
     mov dx,[currentshape]
gamecontinue1:
        mov word[move],15
	mov ah,0
	int 16h
	cmp al,100
	
	jz downn1
	jnz downn2
	
downn1:
	;call sound
	push dx
	call downmovement
    cmp word[move],0
    je gamecontinue2
	jmp rigght2
downn2:
    ;call sound
	cmp al,115
	jz lleft1
	jnz lleft2
lleft1:
   	push dx
	call leftmovement
	jmp rigght2
lleft2:
    ;call sound
	cmp al,102
	jz rigght1
	jnz rigght2
rigght1:
	push dx
	call rightmovement
rigght2:
	cmp word[scoreflag],1
	jne gamecontinue22
	mov word[nextshape1],1
 gamecontinue22:
	
	
	dec word[move]
	cmp word[move],0
	jnz gamecontinue1
    jz gamecontinue2



;;;;;;;;;;;;  storing next shape in the current shape    ;;;;;;;;;;;;;;



gamecontinue2:
	call sound
	push dx
    mov ax,0xb800
    mov es,ax
	mov ax,[x1]
	sub ax,480
	mov di,ax
	cmp word[es:di],0x0720
	jz gamefinish
     mov dx,[nextshape1]
     mov [currentshape],dx
     dec word[terminate]
     jnz gamecontinue

gamefinish:
	jmp endgame

;;;;;;;;;;;;  end screen    ;;;;;;;;;;;;;;




endgame:
    mov word[sec1],2004
    call clrwhite
   ;score printing
    mov ax,30   ;column
    push ax
    mov ax,8   ;rows
    push ax
    mov ax,112   ;colours
    push ax
    mov ax, name5
    push ax
    push word [length5]
    call printstr

    
    mov ax,30   ;column
    push ax
    mov ax,15   ;row
    push ax
    mov ax,112   ;colour
    push ax
    mov ax, name3
    push ax
    push word [length3]
    call printstr

    push word [scroeeee]
    call printnum2
    


;;;;;;;;;;;;  unhooking the timer    ;;;;;;;;;;;;;;



    mov ax,[oldisr]
    mov bx,[oldisr+2]
    cli
    mov [es:8*4],ax
    mov [es:8*4+2],bx
    sti
    

mov ax,0x4c00
int 21h

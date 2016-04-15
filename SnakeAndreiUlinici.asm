;  ##############################################################
; # * * * * * * * * * * * * * * * * * * * * * *                  #
; # *                        Snake Game                          # 
; # * * * * * * * * * * * * * * * * * * * * * >     F            #
;  ##############################################################

;Acest cod scrie direct in memoria video in loc de int 21H (21H este
;mai incet, cu exceptia afisarii scorului si a mesajelor) 
;Programul este incarcat si pe Git:
; https://github.com/andreiulinici/TemaSMP
; Verificati ultima versiune, pentru corectarea bug-urilor

INCLUDE 'emu8086.inc'


left equ 0
top equ 2
row equ 15
col equ 40
right equ left+col
bottom equ top+row

.model small 
;_text segment use16     
     
;org 7C00h ;BIOS-ul va incarca programul nostru la 0x7C00   
    
;start:    

.data          
    msg db "Bine ati venit!",0
    instructiuni db 0AH,0DH,"Pentru a continua, apasati orice tasta!$"
    quitmsg db "Multumesc pentru joc!",0
    gameovermsg db "Ati intrat in zid! Game over! ", 0
    scoremsg db "Scor: ",0
    head db '^',10,10
    body db '*',10,11, 3*15 DUP(0)
    segmentcount db 1
    fruitactive db 1
    fruitx db 8
    fruity db 8
    gameover db 0
    quit db 0   
    delaytime db 5 

.stack
    dw   128  dup(0) 

.code
main proc far
	mov ax, @data
	mov ds, ax 
	
	mov ax, 0b800H
	mov es, ax 
	
	
	; sterge ecranul; se putea si cu procedura CLEAR_SCREEN, predefinita
	mov ax, 0003H
	int 10H       
	
    
	lea bx, msg   ;afiseaza mesaj de bun venit
	mov dx,00
	call writecharat ;apeleaza procedura de scriere caracter
	
    PRINTN 'Introduceti scorul la care credeti ca ajungeti: '    
    CALL scan_num ;cheama procedura de citire numar intre  -32767 si 32767
    PUTC 13 ; trece la inceputul liniei (carriage return)
    PUTC 10 ; trece pe linia urmatoare (line feed)
    PRINT "Scorul dorit: "
    MOV AX, CX ;copiaza numarul in registrul AX
    CALL print_num ;scrie numarul in registrul AX   
    PRINT " .. Nu e rau :) "
    
    PRINTN ' '
    PRINTN 'Folositi W, A, S, D pentru a controla sarpele.'
    PRINTN 'Folositi oricand tasta Q pentru a iesi.'
	
	lea dx, instructiuni
	mov ah, 09H
	int 21h

	
	mov ah, 07h
	int 21h
	mov ax, 0003H
	int 10H
    call printbox ; cheama procedura de desenare zid    
    
    
mainloop:       
    call delay             
    lea bx, msg
    mov dx, 00
    call writestringat
    call shiftsnake
    cmp gameover,1
    je gameover_mainloop
    
    call keyboardfunctions
    cmp quit, 1
    je quitpressed_mainloop
    call fruitgeneration
    call draw
 
    
    jmp mainloop
    
gameover_mainloop: 
    mov ax, 0003H
	int 10H
    mov delaytime, 100
    mov dx, 0000H
    lea bx, gameovermsg
    call writestringat
    call delay    
    jmp quit_mainloop    
    
quitpressed_mainloop:
    mov ax, 0003H
	int 10H    
    mov delaytime, 100
    mov dx, 0000H
    lea bx, quitmsg
    call writestringat
    call delay    
    jmp quit_mainloop       
    

quit_mainloop:
; sterge ecran
mov ax, 0003H
int 10h    
mov ax, 4c00h
int 21h  


delay proc 
    
    ; aceasta procedura foloseste intreruperea 1A 
    mov ah, 00
    int 1Ah
    mov bx, dx
    
jmp_delay:
    int 1Ah
    sub dx, bx
    ; sunt aproximativ 18 tick-uri pe secunda
    ; 10 sunt suficiente
    cmp dl, delaytime                                                      
    jl jmp_delay    
    ret
    
delay endp


fruitgeneration proc
    mov ch, fruity
    mov cl, fruitx
regenerate:
    
    cmp fruitactive, 1
    je ret_fruitactive
    mov ah, 00
    int 1Ah
    ; registrul dx contine tick-urile
    push dx
    mov ax, dx
    xor dx, dx
    xor bh, bh
    mov bl, row
    dec bl
    div bx
    mov fruity, dl
    inc fruity
    
    
    pop ax
    mov bl, col
    dec dl
    xor bh, bh
    xor dx, dx
    div bx
    mov fruitx, dl
    inc fruitx
    
    cmp fruitx, cl
    jne ceva
    cmp fruity, ch
    jne ceva
    jmp regenerate             
ceva:
    mov al, fruitx
    ror al,1
    jc regenerate
    
    
    add fruity, top
    add fruitx, left 
    
    mov dh, fruity
    mov dl, fruitx
    call readcharat
    cmp bl, '*'
    je regenerate
    cmp bl, '^'
    je regenerate
    cmp bl, '<'
    je regenerate
    cmp bl, '>'
    je regenerate
    cmp bl, 'v'
    je regenerate    
    
ret_fruitactive:
    ret
fruitgeneration endp


dispdigit proc
    add dl, '0'
    mov ah, 02H
    int 21H
    ret
dispdigit endp   
   
dispnum proc    
    test ax,ax
    jz retz
    xor dx, dx
    ; ax contine numarul ce urmeaza sa fie afisat
    ; bx trebuie sa contina 10
    mov bx,10
    div bx
    ;dispnum ax first.
    push dx
    call dispnum  
    pop dx
    call dispdigit
    ret
retz:
    mov ah, 02  
    ret    
dispnum endp   



; seteaza pozitia cursorului, ax, bx, dh este linia, dl este coloana
; retine alte registre
setcursorpos proc
    mov ah, 02H
    push bx
    mov bh,0
    int 10h
    pop bx
    ret
setcursorpos endp



draw proc
    lea bx, scoremsg
    mov dx, 0109
    call writestringat
    
    
    add dx, 7
    call setcursorpos
    mov al, segmentcount
    dec al
    xor ah, ah
    call dispnum
        
    lea si, head
draw_loop:
    mov bl, ds:[si]
    test bl, bl
    jz out_draw
    mov dx, ds:[si+1]
    call writecharat
    add si,3   
    jmp draw_loop 

out_draw:
    mov bl, 'F'
    mov dh, fruity
    mov dl, fruitx
    call writecharat
    mov fruitactive, 1
    
    ret    
    
draw endp



; dl contine caracterele ascii la apasare, altfel contine 0
; se foloseste dx si ax
readchar proc
    mov ah, 01H
    int 16H
    jnz keybdpressed
    xor dl, dl
    ret
keybdpressed:
    ;extrage textul introdus de la tastatura
    mov ah, 00H
    int 16H
    mov dl,al
    ret

readchar endp             


keyboardfunctions proc
    
    call readchar
    cmp dl, 0
    je next_14
    
    ; Taste principale
    cmp dl, 'w'
    jne next_11
    cmp head, 'v'
    je next_14
    mov head, '^'
    ret
next_11:
    cmp dl, 's'
    jne next_12
    cmp head, '^'
    je next_14
    mov head, 'v'
    ret
next_12:
    cmp dl, 'a'
    jne next_13
    cmp head, '>'
    je next_14
    mov head, '<'
    ret
next_13:
    cmp dl, 'd'
    jne next_14
    cmp head, '<'
    je next_14
    mov head,'>'
next_14:    
    cmp dl, 'q'
    je quit_keyboardfunctions
    ret    
quit_keyboardfunctions:   
    ; conditii pentru iesire 
    inc quit
    ret
    
keyboardfunctions endp


  
shiftsnake proc     
    mov bx, offset head
    
    ; determina unde se duce capul sarpelui
    ; se retine capul
    xor ax, ax
    mov al, [bx]
    push ax
    inc bx
    mov ax, [bx]
    inc bx    
    inc bx
    xor cx, cx
l:      
    mov si, [bx]
    test si, [bx]
    jz outside
    inc cx     
    inc bx
    mov dx,[bx]
    mov [bx], ax
    mov ax,dx
    inc bx
    inc bx
    jmp l
    
outside:    
    
    
    ; Miscare cap in directia potrivita
    ; Sterge ultimul segment daca sarpele a mancat fructul 
	
    pop ax
    
    push dx
	; dx contine coordonatele ultimului segment si deci
    ; putem sa-l folosim pentru stergere.
    
    ; al contine directia capului
    lea bx, head
    inc bx
    mov dx, [bx]
    
    cmp al, '<'
    jne next_1
    dec dl
    dec dl
    jmp done_checking_the_head
next_1:
    cmp al, '>'
    jne next_2                
    inc dl 
    inc dl
    jmp done_checking_the_head
    
next_2:
    cmp al, '^'
    jne next_3 
    dec dh
    jmp done_checking_the_head
    
next_3:
    ; 'v'
    inc dh
    
    
done_checking_the_head:    
    mov [bx],dx
    ; dx contine pozitia capatului  
    call readcharat ;dx
    ; bl contine rezultatul
    
    cmp bl, 'F'
    je mananca_fruct
    
    ; sterge ultimul segment daca fructul nu e mancat  
    mov cx, dx
    pop dx 
    cmp bl, '#'    ;cazul in care se musca singur => Game Over
    je game_over
    mov bl, 0
    call writecharat
    mov dx, cx
    

	
    ; verifica daca sarpele s-a lovit de zid
    cmp dh, top
    je game_over
    cmp dh, bottom
    je game_over
    cmp dl,left
    je game_over
    cmp dl, right
    je game_over
    
    ret
game_over:
    inc gameover
    
	; sunet: 5 beep-uri
    mov dl, 07h
    mov ah, 2
    int 21h
    mov dl, 07h
    mov ah, 2
    int 21h
    mov dl, 07h
    mov ah, 2
    int 21h
    mov dl, 07h
    mov ah, 2
    int 21h
    mov dl, 07h
    mov ah, 2
    int 21h
    
    ret
mananca_fruct:

    ;mov dl, 07h  ;beep
    ;mov ah, 2
    ;int 21h    

    ; adauga segment
    mov al, segmentcount
    xor ah, ah
    
    
    lea bx, body
    mov cx, 3
    mul cx
    
    pop dx
    add bx, ax
    mov byte ptr ds:[bx], '*'
    mov [bx+1], dx
    inc segmentcount 
    mov dh, fruity
    mov dl, fruitx
    mov bl, 0
    call writecharat
    mov fruitactive, 0   
    ret 
shiftsnake endp
   
   

; Printbox
printbox proc
    

; Deseneaza zid
    mov dh, top
    mov dl, left
    mov cx, col
    mov bl, '#'
l1:                 
    call writecharat
    inc dl
    loop l1
    
    mov cx, row
l2:
    call writecharat
    inc dh
    loop l2
    
    mov cx, col
l3:
    call writecharat
    dec dl
    loop l3

    mov cx, row     
l4:
    call writecharat    
    dec dh 
    loop l4    
    
    ret
printbox endp


; dx contine linii si coloane
; bl contine caractere
; foloseste di. 
writecharat proc
    ;80x25
    push dx
    mov ax, dx
    and ax, 0FF00H
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    
    
    push bx
    mov bh, 160
    mul bh 
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
    mov es:[di], bl
    pop dx
    ret    
writecharat endp
 
 
; dx contine linii si coloane
; returneaza caracter la bl
; foloseste registrul di
readcharat proc
    push dx
    mov ax, dx
    and ax, 0FF00H
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1    
    push bx
    mov bh, 160
    mul bh 
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
    mov bl,es:[di]
    pop dx
    ret
readcharat endp        


; dx contine linie, coloana
; bx contine offset-ul stringului
writestringat proc
    push dx
    mov ax, dx
    and ax, 0FF00H
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    shr ax,1
    
    push bx
    mov bh, 160
    mul bh
    
    pop bx
    and dx, 0FFH
    shl dx,1
    add ax, dx
    mov di, ax
loop_writestringat:
    
    mov al, [bx]
    test al, al
    jz exit_writestringat
    mov es:[di], al
    inc di
    inc di
    inc bx
    jmp loop_writestringat
    
    
exit_writestringat:
    pop dx
    ret
    
    
writestringat endp


DEFINE_SCAN_NUM
DEFINE_PRINT_STRING
DEFINE_PRINT_NUM
DEFINE_PRINT_NUM_UNS     
     
;main endp
          
end main

; CENG336 HW2
    ; Ulku Meteriz
    ; Necip Fazil Yildiran

list P=18f8722
#include <p18f8722.inc>
config OSC = HSPLL, FCMEN = OFF, IESO = OFF, PWRT = OFF, BOREN = OFF, WDT = OFF, MCLRE = ON, LPT1OSC = OFF, LVP = OFF, XINST = OFF, DEBUG = OFF

phaseInfo udata 0x20
phaseInfo

NOF udata 0x21 ; number of fruits
NOF

NOFL udata 0x22 ; number of fruits low
NOFL

NOFH udata 0x23 ; number of fruits high
NOFH

lives udata 0x24
lives

level udata 0x25
level

displayState udata 0x26
displayState

delay256Var udata 0x27
delay256Var

delay256Var2 udata 0x28
delay256Var2

; end: coordinates

; 7th bit: if 1, NO fruit
;          if 0, fruit exists
; count is kept in [0-3] by incrementing
c00 udata 0x29
c00

c01 udata 0x30
c01

c02 udata 0x31
c02

c03 udata 0x32
c03

c10 udata 0x33
c10

c11 udata 0x34
c11

c12 udata 0x35
c12

c13 udata 0x36
c13

c20 udata 0x37
c20

c21 udata 0x38
c21

c22 udata 0x39
c22

c23 udata 0x40
c23

c30 udata 0x41
c30

c31 udata 0x42
c31

c32 udata 0x43
c32

c33 udata 0x44
c33
; end: coordinates

seed udata 0x45
seed

score udata 0x46
score

; 7th bit -> south
; 6th bit -> north
; 5th bit -> west
; 4th bit -> east
myDirection udata 0x47
myDirection

; bitwise
myRow udata 0x48
myRow

; bitwise
myCol udata 0x49
myCol

intrCounter udata 0x50
intrCounter


; newFruitFlag         : 0
; moveFlag             : 1
; updateFruitCountFlag : 2
; controlCountFlag     : 3
; gameTimeIncrFlag     : 4
; reflectLiveFlag      : 5
; isEatenFlag	       : 6
; toggleFlag           : 7
flags udata 0x51
flags

gameTime udata 0x52
gameTime

gameTimeH udata 0x53
gameTimeH

gameTimeL udata 0x54
gameTimeL

timeScaler udata 0x55 
timeScaler

; level0 -> 4
; level1 -> 8
; level2 -> 16
; level3 -> 32
; indicates t/8 time
readTimeScaler udata 0x56
readTimeScaler
;levelIndicator -> readTimeScaler
levelIndicator udata 0x57
levelIndicator

elapsedT8 udata 0x58
elapsedT8

isSnakeOn udata 0x59
isSnakeOn

remainingLives udata 0x60
remainingLives

dummyVar udata 0x61
dummyVar

portBVar udata 0x62
portBVar

w_temp  udata 0x63
w_temp

status_temp udata 0x64
status_temp

pclath_temp udata 0x65
pclath_temp

scoreL udata 0x66
scoreL

scoreH udata 0x67
scoreH

phase3success udata 0x68
phase3success

gameTimeControllerVar udata 0x69
gameTimeControllerVar

scNOF udata 0x70
scNOF

scNOFL udata 0x71
scNOFL

scNOFH udata 0x72
scNOFH

org 0x00
    goto init

org 0x08
    goto isr

org 0x18
    goto lowIsr



displayTable
    rlncf WREG, W	    ; multiply index by 2
    addwf PCL, 1	    ; modify program counter
    retlw b'00111111'	    ; 0
    retlw b'00000110'	    ; 1
    retlw b'01011011'	    ; 2
    retlw b'01001111'	    ; 3
    retlw b'01100110'	    ; 4
    retlw b'01101101'	    ; 5
    retlw b'01111101'	    ; 6
    retlw b'00000111'	    ; 7
    retlw b'01111111'	    ; 8
    retlw b'01100111'	    ; 9
    retlw b'00111110'       ; U


saveRegisters
    movwf 	w_temp
    swapf 	STATUS, w
    clrf 	STATUS
    movwf 	status_temp
    movf 	PCLATH, w
    movwf 	pclath_temp
    clrf 	PCLATH
    return

restoreRegisters
    movf 	pclath_temp, w
    movwf 	PCLATH
    swapf 	status_temp, w
    movwf 	STATUS
    swapf 	w_temp, f
    swapf 	w_temp, w
    return

delay256
    movlw d'255'
    movwf delay256Var
    movwf delay256Var2

delay256Loop2:
    dcfsnz delay256Var2
    return

    movwf delay256Var
delay256Loop1:
    decfsz delay256Var
    goto delay256Loop1

    goto delay256Loop2
    return


; 7th bit -> south
; 6th bit -> north
; 5th bit -> west
; 4th bit -> east

changeDirEast
    movlw b'00010000'
    movwf myDirection
    return

changeDirWest
    movlw b'00100000'
    movwf myDirection
    return

changeDirNorth
    movlw b'01000000'
    movwf myDirection
    return

changeDirSouth
    movlw b'10000000'
    movwf myDirection
    return

B4released
    movf portBVar, 0
    xorwf PORTB, W         ; XOR portBVar and PORTB, write result in WREG
    btfsc WREG, 4
        call changeDirEast
    return

B5released
    movf portBVar, 0
    xorwf PORTB, W         ; XOR portBVar and PORTB, write result in WREG
    btfsc WREG, 5
        call changeDirWest
    return

B6released
    movf portBVar, 0
    xorwf PORTB, W         ; XOR portBVar and PORTB, write result in WREG
    btfsc WREG, 6
        call changeDirNorth
    return

B7released
    movf portBVar, 0
    xorwf PORTB, W
    btfsc WREG, 7
        call changeDirSouth
    return

lowIsr
    call saveRegisters

    ; now, indicate which button is pressed
    btfss PORTB, 4
        call B4released
    btfss PORTB, 5
        call B5released
    btfss PORTB, 6
        call B6released
    btfss PORTB, 7
        call B7released

    ; clear interrupt
    movf PORTB, W
    movwf portBVar      ; save PORTB's last value in portBVar
    clrf PORTB
    bcf INTCON, 0       ; clear PORTB interrupt flag

    call restoreRegisters
    retfie

isr
    call saveRegisters

    ; phase1? phase2;
    btfsc phaseInfo, 0
        call phase1isr
    btfsc phaseInfo, 1
        call phase2isr
    btfsc phaseInfo, 2
        call phase3isr

    call restoreRegisters
    retfie

phase1isr
    call updateDisplayPhase1
    bcf INTCON, TMR0IF
    movlw b'11111100'
    movwf TMR0L

    return

phase2isr
    nop
    movf PORTB, w
    nop
    btfsc INTCON, TMR0IF
        call phase2timer0isr
    return

phase3isr
    call updateDisplayPhase3
    bcf INTCON, TMR0IF
    movlw b'11111100'
    movwf TMR0L

    return


phase2timer0isr
    ; clear TMR0 interrupt flag
    bcf INTCON, TMR0IF

    decfsz timeScaler
        return
    goto timeScalerZero


    timeScalerZero:
        ; init timeScaler by readTimeScaler
        movf readTimeScaler, 0
        movwf timeScaler

        incf elapsedT8

        call flagHandler
        return

flagHandler
    call Tover8flags

    ; is T/2
    movlw d'4'
    cpfseq elapsedT8
	goto otherTover2check
    goto Tover2isDone

    otherTover2check:
	movlw d'8'
	cpfseq elapsedT8
	    return
	goto Tover2isDone

    Tover2isDone:
        call Tover2flags
        btfsc elapsedT8, 3
            goto TisDone
        return

    TisDone:
        call Tflags
	clrf elapsedT8
	return

Tover8flags
    bsf flags, 7       ; toggleFlag
    return

Tover2flags
    ; game time increment control
    btfsc levelIndicator, 3
        call incGT

    bsf flags, 1       ; moveFlag
    return

Tflags
    ; game time increment control
    call gameTimeController

    bsf flags, 2        ; updateFruitCountFlag
    bsf flags, 0        ; newFruitFlag

    return

gameTimeController
    btfsc levelIndicator, 2
        call incGT
    btfsc levelIndicator, 1
        goto level1caseForGT
    btfsc levelIndicator, 0
        goto level0caseForGT

    level1caseForGT:
        dcfsnz gameTimeControllerVar
            call incGT
        return
    level0caseForGT:
        dcfsnz gameTimeControllerVar
            call incGT
        return

initGameTimeController
    movlw b'00000010'
    movwf gameTimeControllerVar
    btfsc levelIndicator, 0
        rlncf gameTimeControllerVar
    return
;T2flags
;    return

;T4flags
;    return


updateDisplayPhase1
    btfsc displayState, 0
    goto display0
    btfsc displayState, 1
    goto display1
    btfsc displayState, 2
    goto display2
    btfsc displayState, 3
    goto display3

display0:
	    ; level -> display
    movf level, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000001'
    movwf LATH

	    ; next state
    movlw b'00000010'
    movwf displayState
    return
display1:
	    ; lives -> display
    movf lives, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000010'
    movwf LATH

	    ; next state
    movlw b'00000100'
    movwf displayState
    return
display2:
	    ; NOFH -> display
    movf NOFH, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000100'
    movwf LATH

	    ; next state
    movlw b'00001000'
    movwf displayState
    return
display3:
	    ; NOFL -> display
    movf NOFL, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00001000'
    movwf LATH

	    ; next state
    movlw b'00000001'
    movwf displayState
    return


updateDisplayPhase2
    call setscNOF

    btfsc displayState, 0
    goto display0Phase2
    btfsc displayState, 1
    goto display1Phase2
    btfsc displayState, 2
    goto display2Phase2
    btfsc displayState, 3
    goto display3Phase2

display0Phase2:
	    ; scNOFH -> display
    movf scNOFH, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000001'
    movwf LATH

	    ; next state
    movlw b'00000010'
    movwf displayState
    return
display1Phase2:
	    ; scNOFL -> display
    movf scNOFL, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000010'
    movwf LATH

	    ; next state
    movlw b'00000100'
    movwf displayState
    return
display2Phase2:
	    ; gameTimeH -> display
    movf gameTimeH, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000100'
    movwf LATH

	    ; next state
    movlw b'00001000'
    movwf displayState
    return
display3Phase2:
	    ; gameTimeL -> display
    movf gameTimeL, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00001000'
    movwf LATH

	    ; next state
    movlw b'00000001'
    movwf displayState
    return


updateDisplayPhase3
    btfsc displayState, 0
    goto display0Phase3
    btfsc displayState, 1
    goto display1Phase3
    btfsc displayState, 2
    goto display2Phase3
    btfsc displayState, 3
    goto display3Phase3

display0Phase3:
	    ; phase3success -> display
    movf phase3success, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000001'
    movwf LATH

	    ; next state
    movlw b'00000010'
    movwf displayState
    return
display1Phase3:
	    ; lives -> display
    movf lives, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000010'
    movwf LATH

	    ; next state
    movlw b'00000100'
    movwf displayState
    return
display2Phase3:
	    ; scoreH -> display
    movf scoreH, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00000100'
    movwf LATH

	    ; next state
    movlw b'00001000'
    movwf displayState
    return
display3Phase3:
	    ; scoreL -> display
    movf scoreL, 0
    call displayTable
    movwf LATJ

	    ; RH setting
    movlw b'00001000'
    movwf LATH

	    ; next state
    movlw b'00000001'
    movwf displayState
    return

init
    call phase1init
    goto main



phase1init
    ; about interrupts
    clrf INTCON
    clrf INTCON2
    clrf T0CON		; configuration of Timer-0. Not on yet
    movlw b'00001000'
    movwf T0CON	
    bsf T0CON, TMR0ON	; Timer-0: on

    ; display state
    movlw b'00000001'
    movwf displayState

    ; phase state
    clrf phaseInfo
    bsf phaseInfo, 0

    ; clear all latches to turn off leds
    clrf LATA
    clrf LATB
    clrf LATC
    clrf LATD
    clrf LATE
    clrf LATF
    clrf LATG
    clrf LATH
    clrf LATJ

    movlw b'00010000'
    movwf TRISA		    ; RA4: input
    movlw b'00001111'
    movwf ADCON1	    ; RA4: digital input
    movlw b'00111111'
    movwf TRISC		    ; RC[0-5]: input
    movlw b'11110000'
    movwf TRISH		    ; RH[0-3]: output
    movlw b'00000000'
    movwf TRISJ		    ; RJ[0-7]: output

    movlw b'00000001'	    ; phase: phase1
    movwf phaseInfo

    ; number of fruits: 15
    movlw d'15'
    movwf NOF
    movlw d'1'
    movwf NOFH
    movlw d'5'
    movwf NOFL

    movlw d'3'
    movwf lives		    ; lives: 3
    movwf level		    ; level: 3

    movlw b'10100000'	    ; global interrupt: 1, timer0 interrupt: 1
    movwf INTCON
    call updateDisplayPhase1

    return

phase2init
    ; about interrupts
    clrf INTCON
    clrf INTCON2
    clrf T0CON

    ; clear all latches
    clrf LATA
    clrf LATB
    clrf LATC
    clrf LATD
    clrf LATE
    clrf LATF
    clrf LATG
    clrf LATH
    clrf LATJ

    ; phaseInfo: phase2
    movlw b'00000010'
    movwf phaseInfo

    ; display state
    movlw b'00000001'
    movwf displayState

    ; I/O
    clrf TRISA
    clrf TRISC
    clrf TRISD
    clrf TRISE
    clrf TRISF
    clrf TRISJ

    movlw b'11110000'
    movwf TRISH		    ; RH[0-3]: output
    movwf TRISB		    ; RB[4-7]: interrupt input

    ; variable init

    ; coordinate variables
    movlw b'10000000' ; no fruit exists initially, therefore, 7th bit is set to 1, others to 0
    movwf c00
    movwf c01
    movwf c02
    movwf c03
    movwf c10
    movwf c11
    movwf c12
    movwf c13
    movwf c20
    movwf c21
    movwf c22
    movwf c23
    movwf c30
    movwf c31
    movwf c32
    movwf c33

    ; seed
    movlw 0xAD
    movwf seed

    ; initial score: 0
    clrf score
    clrf scoreH
    clrf scoreL

    ; initial direction: south, 7th bit
    movlw d'10000000'
    movwf myDirection

    ; initial coordinate: (0,0)
    clrf myRow
    clrf myCol
    bsf myRow, 0
    bsf myCol, 0

    ; initial interrupt counter: 0
    clrf intrCounter

    ; initial gameTime: 0
    clrf gameTime
    clrf gameTimeH
    clrf gameTimeL

    ; elapsedT8
    clrf elapsedT8

    ; isSnakeOn
    clrf isSnakeOn
    bsf isSnakeOn, 0 ; initially on

    ; reflectRemainingLives
    bsf flags, 5

    ; initial timer0 settings
    movlw d'19'         ; initially, for level 0, timer0 should interrupt 12 times
    movwf timeScaler

    ; shift the timeScaler to the left for 'level' times, means, timeScaler *= level
    movf level, 0
    incf WREG

    decLoopForTimeScalerSetting:
        dcfsnz WREG
            goto decLoopForTimeScalerSettingDone
        rlncf timeScaler
        goto decLoopForTimeScalerSetting
    decLoopForTimeScalerSettingDone:

    movf timeScaler, 0            ; you may use it as variable, then, set it to readTimeScaler
    movwf readTimeScaler        ; it is for reading, don't change or count using this

    ; prepare a bitwise level indicator to use later
    clrf levelIndicator
    bsf levelIndicator, 0
    movf level, 0
    incf WREG

    decLoopForLevelIndicatorSetting:
        dcfsnz WREG
            goto decLoopForLevelIndicatorSettingDone
        rlncf levelIndicator
        goto decLoopForLevelIndicatorSetting
    decLoopForLevelIndicatorSettingDone:

    call initGameTimeController

    clrf flags
    bsf flags, 5

    ; initially, im at position 0,0 (RC0)
    call lightOn

    movf NOF, 0
    movwf scNOF

    movf NOFL, 0
    movwf scNOFL

    movf NOFH, 0
    movwf scNOFH

    call intrConfig
    return


intrConfig
	movlw b'01000101'   ; prescaler assigned
	movwf T0CON

    bsf RCON, IPEN	    ; enable interrupt priority

	bcf INTCON2, 0	    ; RB interrupt <- low priority
	bsf INTCON2, 2	    ; TMR0 <- high priority
	bsf INTCON2, 7	    ; enable external pull-up interrupt
	bsf INTCON, 3	    ; enable RB Port change
	bsf INTCON, 5	    ; enable TMR0 overflow interrupt

    ; clear PORTB interrupt flag
    movf PORTB, W
    clrf PORTB
    bcf INTCON, 0

    ; set initial PORTB value
    movf PORTB, 0
    movwf portBVar

	bsf INTCON, 6	    ; enable low priority interrupt
    bsf INTCON, 7	    ; enable high priority interrupt
	bsf T0CON, 7	    ; enable timer

	return


; start: RC0 polling for phase1
isRC0Pressed
    btfsc PORTC, 0
    call RC0pressed
    return

RC0pressed
    call waitRC0release
    movlw d'0'
    cpfseq NOF; compare f with working register, skip if equal
    call decNOF
    call delay256
    return

waitRC0release
    call delay256
    btfsc PORTC, 0
    bra waitRC0release
    return
; end: RC0 polling for phase1

; start: RC1 polling for phase1
isRC1Pressed
    btfsc PORTC, 1
    call RC1pressed
    return

RC1pressed
    call waitRC1release
    movlw d'99'
    cpfseq NOF; compare f with working register, skip if equal
    call incNOF
    call delay256
    return

waitRC1release
    call delay256
    btfsc PORTC, 1
    bra waitRC1release
    return
; end: RC1 polling for phase1

; start: RC2 polling for phase1
isRC2Pressed
    btfsc PORTC, 2
    call RC2pressed
    return

RC2pressed
    call waitRC2release
    movlw d'0'
    cpfseq lives    ; compare f with working register, skip if equal
    decf lives
    call delay256
    return

waitRC2release
    call delay256
    btfsc PORTC, 2
    bra waitRC2release
    return
; end: RC2 polling for phase1

; start: RC3 polling for phase1
isRC3Pressed
    btfsc PORTC, 3
    call RC3pressed
    return

RC3pressed
    call waitRC3release
    movlw d'6'
    cpfseq lives    ; compare f with working register, skip if equal
    incf lives
    call delay256
    return

waitRC3release
    call delay256
    btfsc PORTC, 3
    bra waitRC3release
    return
; end: RC3 polling for phase1

; start: RC4 polling for phase1
isRC4Pressed
    btfsc PORTC, 4
    call RC4pressed
    return

RC4pressed
    call waitRC4release
    movlw d'0'
    cpfseq level    ; compare f with working register, skip if equal
    decf level
    call delay256
    return

waitRC4release
    call delay256
    btfsc PORTC, 4
    bra waitRC4release
    return
; end: RC4 polling for phase1

; start: RC5 polling for phase1
isRC5Pressed
    btfsc PORTC, 5
    call RC5pressed
    return

RC5pressed
    call waitRC5release
    movlw d'3'
    cpfseq level    ; compare f with working register, skip if equal
    incf level
    call delay256
    return

waitRC5release
    call delay256
    btfsc PORTC, 5
    bra waitRC5release
    return
; end: RC5 polling for phase1


; start: RA4 polling for phase1
isRA4Pressed
    btfsc PORTA, 4
    call RA4pressed
    return

RA4pressed
    call waitRA4release
    call delay256
    call phase2init
    return

waitRA4release
    call delay256
    btfsc PORTA, 4
    bra waitRA4release
    return
; end: RA4 polling for phase1

incNOF
    incf NOF
    movlw d'9'
    cpfseq NOFL
    goto LincNOF
    goto HincNOF
HincNOF:
    incf NOFH
    clrf NOFL
    return
LincNOF:
    incf NOFL
    return

incscNOF
    incf scNOF
    movlw d'9'
    cpfseq scNOFL
    goto LincscNOF
    goto HincscNOF
HincscNOF:
    incf scNOFH
    clrf scNOFL
    return
LincscNOF:
    incf scNOFL
    return

decNOF
    decf NOF
    movlw d'0'
    cpfseq NOFL
    goto LdecNOF
    goto HdecNOF
HdecNOF:
    decf NOFH
    movlw d'9'
    movwf NOFL
    return
LdecNOF:
    decf NOFL
    return

incGT
    call initGameTimeController
    incf gameTime
    movlw d'9'
    cpfseq gameTimeL
    goto LincGT
    goto HincGT
HincGT:
    clrf gameTimeL

    movlw d'9'
    cpfseq gameTimeH
        goto HincGTnot9
    goto HincGTis9

    HincGTnot9:
        incf gameTimeH
        return
    HincGTis9:
        clrf gameTimeH
        return
LincGT:
    incf gameTimeL
    return

incScore
    incf score
    movlw d'9'
    cpfseq scoreL
    goto LincScore
    goto HincScore
HincScore:
    incf scoreH
    clrf scoreL
    return
LincScore:
    incf scoreL
    return


phase1
    call isRC0Pressed
    call isRC1Pressed
    call isRC2Pressed
    call isRC3Pressed
    call isRC4Pressed
    call isRC5Pressed
    call isRA4Pressed
    return

move
    btfss flags, 1         ; moveFlag
        return
    ;else

    btfsc flags, 0  ; newFruitFlag check
        return

    ; start move process
    bcf flags, 1           ; clear moveFlag

    bsf flags, 5           ; set remainingLivesFlag
    bsf flags, 6           ; set isEatenFlag


    btfsc myDirection, 7   ; south
        goto moveSouth
    btfsc myDirection, 6   ; north
        goto moveNorth
    btfsc myDirection, 5   ; west
        goto moveWest
    btfsc myDirection, 4   ; east
        goto moveEast

    return                  ; shouldn't execute

        moveSouth:
            btfsc myRow, 3
                goto iDied ; move is not possible, im dead!

            ; i didn't die
            rlncf myRow     ; move is done!
            goto iMoved

        moveNorth:
            btfsc myRow, 0
                goto iDied

            ; i didn't die
            rrncf myRow     ; move is done!
            goto iMoved

        moveWest:
            btfsc myCol, 0
                goto iDied

            ; i didn't die
            rrncf myCol     ; move is done!
            goto iMoved

        moveEast:
            btfsc myCol, 3
                goto iDied

            ; i didn't die
            rlncf myCol     ; move is done!
            goto iMoved


        iDied:
            movlw d'0'
            cpfseq lives
                decf lives
            bsf flags, 5    ; reflectLiveFlag
            return
        iMoved:
            call lightOn
            bsf flags, 6    ; isEatenFlag
            return


lightOff
    movf myRow, 0         ; put myRow to W
    comf WREG		  ; take complement of W (myRow)
    btfsc myCol, 0         ; RC
        andwf LATC, 1
    btfsc myCol, 1         ; RD
        andwf LATD, 1
    btfsc myCol, 2         ; RE
        andwf LATE, 1
    btfsc myCol, 3         ; RF
        andwf LATF, 1
    return

lightOn
    movf myRow, 0         ; put myRow to W
    btfsc myCol, 0         ; RC
        iorwf LATC, 1
    btfsc myCol, 1         ; RD
        iorwf LATD, 1
    btfsc myCol, 2         ; RE
        iorwf LATE, 1
    btfsc myCol, 3         ; RF
        iorwf LATF, 1
    return

reflectRemainingLives
    btfss flags, 5         ; reflectLiveFlag
        return

    ;else
    bcf flags, 5            ; clear reflectLiveFlag
    clrf remainingLives
    incf remainingLives
    movlw d'0'
    cpfseq lives
        goto itIsNotDeadReflectRemainingLives

    clrf LATA
    return

    itIsNotDeadReflectRemainingLives:
        movf lives, 0

        remainingLivesLoop:
            dcfsnz WREG
                goto nowReflectTheLives
            rlncf remainingLives
            incf remainingLives
            goto remainingLivesLoop

    nowReflectTheLives:
        movf remainingLives, 0
        movwf LATA

    return


setscNOF
    ; set number of fruits 'not generated yet'
    movf NOF, 0
    movwf scNOF

    movf NOFL, 0
    movwf scNOFL

    movf NOFH, 0
    movwf scNOFH

    ; now add the fruits on the screen
    call numOfFruitOnScreen

    movwf dummyVar
    incf dummyVar

    setscNOFLoop:
        dcfsnz dummyVar
            goto setscNOFLoopDone
        call incscNOF
        goto setscNOFLoop
    setscNOFLoopDone:
    
    return

isGameOver

    movlw d'0'
    cpfseq lives
        goto otherGameOverChecks
    ;else, game is over
    call phase3init
    clrf flags
    return

    otherGameOverChecks:
    ; NOF , ekranda var m?
    cpfseq NOF
	return
    call numOfFruitOnScreen
    movwf dummyVar
    movlw d'0'
    cpfseq dummyVar
	return
    call phase3init
    clrf flags
    return


numOfFruitOnScreen

    clrf WREG

    btfss c00, 7
        incf WREG
    btfss c01, 7
        incf WREG
    btfss c02, 7
        incf WREG
    btfss c03, 7
        incf WREG
    btfss c10, 7
        incf WREG
    btfss c11, 7
        incf WREG
    btfss c12, 7
        incf WREG
    btfss c13, 7
        incf WREG
    btfss c20, 7
        incf WREG
    btfss c21, 7
        incf WREG
    btfss c22, 7
        incf WREG
    btfss c23, 7
        incf WREG
    btfss c30, 7
        incf WREG
    btfss c31, 7
        incf WREG
    btfss c32, 7
        incf WREG
    btfss c33, 7
        incf WREG

    return

toggleLight
    btfss flags, 7      ; toggleLightFlag
        return

    bcf flags, 7
    ;else
    btfsc isSnakeOn, 0
        goto timeToOff
    goto timeToOn

    timeToOff:
        call lightOff
        bcf isSnakeOn, 0
        return

    timeToOn:
        call lightOn
        bsf isSnakeOn, 0
        return


updateCount
	; isEaten??
    btfss flags, 2      ; updateFruitCountFlag
        return
    ;else
    bcf flags, 2
    bsf flags, 3        ; controlCountFlag

    btfss c00, 7
        incf c00
    btfss c01, 7
        incf c01
    btfss c02, 7
        incf c02
    btfss c03, 7
        incf c03
    btfss c10, 7
        incf c10
    btfss c11, 7
        incf c11
    btfss c12, 7
        incf c12
    btfss c13, 7
        incf c13
    btfss c20, 7
        incf c20
    btfss c21, 7
        incf c21
    btfss c22, 7
        incf c22
    btfss c23, 7
        incf c23
    btfss c30, 7
        incf c30
    btfss c31, 7
        incf c31
    btfss c32, 7
        incf c32
    btfss c33, 7
        incf c33

    return

controlCounts
    ; isEaten??
    btfss flags, 3      ; controlCountFlag
        return
    ;else
    bcf flags, 3        ; clear flag

    movlw b'10000000'

    btfsc c00, 2
        movwf c00
    btfsc c01, 2
        movwf c01
    btfsc c02, 2
        movwf c02
    btfsc c03, 2
        movwf c03
    btfsc c10, 2
        movwf c10
    btfsc c11, 2
        movwf c11
    btfsc c12, 2
        movwf c12
    btfsc c13, 2
        movwf c13
    btfsc c20, 2
        movwf c20
    btfsc c21, 2
        movwf c21
    btfsc c22, 2
        movwf c22
    btfsc c23, 2
        movwf c23
    btfsc c30, 2
        movwf c30
    btfsc c31, 2
        movwf c31
    btfsc c32, 2
        movwf c32
    btfsc c33, 2
        movwf c33

    return

newFruit
    ; yeni koyulacak olan?n count u hemen update edilmemeli
    ; updateCount, ControlCount bekle

    btfss flags, 0      ; newFruitFlag
        return
    ;else

    btfsc flags, 3	; controlCountFlag check
	return
    btfsc flags, 2	; updateFruitCountFlag check
	return

	; start newFruit process
    bcf flags, 0        ; clear flag



    movlw d'0'
    cpfsgt NOF	; if NOF is greater than  0 continue
	return	; else return

    call decNOF ; NOF will be decremented

    movlw 0xB8          ; to XOR with seed

    rlncf seed, 1
    btfsc seed, 0     ; control carry bit
        xorwf seed, 1   ; XOR(seed, 0xB8)

    movlw b'00000011'   ; to mask the columnIndex
    movwf dummyVar

    movf seed, 0
    andwf dummyVar, 1   ; to keep columnIndex in dummyVar

    movlw b'0'
    cpfseq dummyVar     ; if columnIndex == 0, skip
        goto colIsNotZero
    goto colIsZero

    colIsNotZero:
        dcfsnz dummyVar
            goto colIsOne
        dcfsnz dummyVar
            goto colIsTwo
        dcfsnz dummyVar
            goto colIsThree

        return          ; shouldn't execute

    colIsZero:
	movf seed, 0
	movwf dummyVar
	movlw b'11000000'
	andwf dummyVar, 1
	rlncf dummyVar  ; rotate 2 times left to get row index
	rlncf dummyVar

	movlw b'00000000' ; there is a fruit and count is zero

	incf dummyVar

	dcfsnz dummyVar
	    movwf c00	; c00
	dcfsnz dummyVar
	    movwf c10	; c10
	dcfsnz dummyVar
	    movwf c20	; c20
	dcfsnz dummyVar
	    movwf c30	; c30

	call updateFruitLights

        return
    colIsOne:

	movf seed, 0
	movwf dummyVar
	movlw b'11000000'
	andwf dummyVar, 1
	rlncf dummyVar  ; rotate 2 times left to get row index
	rlncf dummyVar

	movlw b'00000000' ; there is a fruit and count is zero

	incf dummyVar

	dcfsnz dummyVar
	    movwf c01	; c01
	dcfsnz dummyVar
	    movwf c11	; c11
	dcfsnz dummyVar
	    movwf c21	; c21
	dcfsnz dummyVar
	    movwf c31	; c31

	call updateFruitLights

        return
    colIsTwo:
	movf seed, 0
	movwf dummyVar
	movlw b'11000000'
	andwf dummyVar, 1
	rlncf dummyVar  ; rotate 2 times left to get row index
	rlncf dummyVar

	movlw b'00000000' ; there is a fruit and count is zero

	incf dummyVar

	dcfsnz dummyVar
	    movwf c02	; c02
	dcfsnz dummyVar
	    movwf c12	; c12
	dcfsnz dummyVar
	    movwf c22	; c22
	dcfsnz dummyVar
	    movwf c32	; c32

	call updateFruitLights

        return
    colIsThree:
	movf seed, 0
	movwf dummyVar
	movlw b'11000000'
	andwf dummyVar, 1
	rlncf dummyVar  ; rotate 2 times left to get row index
	rlncf dummyVar

	movlw b'00000000' ; there is a fruit and count is zero

	incf dummyVar

	dcfsnz dummyVar
	    movwf c03	; c03
	dcfsnz dummyVar
	    movwf c13	; c13
	dcfsnz dummyVar
	    movwf c23	; c23
	dcfsnz dummyVar
	    movwf c33	; c33
	call updateFruitLights

	return


updateFruitLights
	; lighton
	btfss c00, 7
	    bsf LATC, 0    ;light on for PORTC
	btfss c10, 7
	    bsf LATC, 1
	btfss c20, 7
	    bsf LATC, 2
	btfss c30, 7
	    bsf LATC, 3
	btfss c01, 7
	    bsf LATD, 0    ;light on for PORTD
	btfss c11, 7
	    bsf LATD, 1
	btfss c21, 7
	    bsf LATD, 2
	btfss c31, 7
	    bsf LATD, 3
	btfss c02, 7
	    bsf LATE, 0    ;light on for PORTE
	btfss c12, 7
	    bsf LATE, 1
	btfss c22, 7
	    bsf LATE, 2
	btfss c32, 7
	    bsf LATE, 3
	btfss c03, 7
	    bsf LATF, 0    ;light on for PORTF
	btfss c13, 7
	    bsf LATF, 1
	btfss c23, 7
	    bsf LATF, 2
	btfss c33, 7
	    bsf LATF, 3

	; lightoff
	btfsc c00, 7
	    bcf LATC, 0    ;light off for PORTC
	btfsc c10, 7
	    bcf LATC, 1
	btfsc c20, 7
	    bcf LATC, 2
	btfsc c30, 7
	    bcf LATC, 3
	btfsc c01, 7
	    bcf LATD, 0    ;light off for PORTD
	btfsc c11, 7
	    bcf LATD, 1
	btfsc c21, 7
	    bcf LATD, 2
	btfsc c31, 7
	    bcf LATD, 3
	btfsc c02, 7
	    bcf LATE, 0    ;light off for PORTE
	btfsc c12, 7
	    bcf LATE, 1
	btfsc c22, 7
	    bcf LATE, 2
	btfsc c32, 7
	    bcf LATE, 3
	btfsc c03, 7
	    bcf LATF, 0    ;light off for PORTF
	btfsc c13, 7
	    bcf LATF, 1
	btfsc c23, 7
	    bcf LATF, 2
	btfsc c33, 7
	    bcf LATF, 3

	btfsc isSnakeOn, 0
	    call lightOn
	return

isEaten
	; kendi row col una bak , meyve varsa sil score artt?r updateLights ça??r return
	btfss flags, 6
	    return

	btfsc flags, 0	    ;wait for newfruitFlag
	    return

	;start isEaten process

	bcf flags, 6

	movlw b'10000000'   ; in case of fruit is eaten

	btfsc myCol, 0	     ; col is 0
	    goto colIs0
	btfsc myCol, 1	    ; col is 1
	    goto colIs1
	btfsc myCol, 2	     ; col is 2
	    goto colIs2
	btfsc myCol, 3	    ; col is 3
	    goto colIs3


    colIs0:
	btfsc myRow, 0
	    goto c00Control
	btfsc myRow, 1
	    goto c10Control
	btfsc myRow, 2
	    goto c20Control
	btfsc myRow, 3
	    goto c30Control
    colIs1:
	btfsc myRow, 0
	    goto c01Control
	btfsc myRow, 1
	    goto c11Control
	btfsc myRow, 2
	    goto c21Control
	btfsc myRow, 3
	    goto c31Control
    colIs2:
	btfsc myRow, 0
	    goto c02Control
	btfsc myRow, 1
	    goto c12Control
	btfsc myRow, 2
	    goto c22Control
	btfsc myRow, 3
	    goto c32Control
    colIs3:
	btfsc myRow, 0
	    goto c03Control
	btfsc myRow, 1
	    goto c13Control
	btfsc myRow, 2
	    goto c23Control
	btfsc myRow, 3
	    goto c33Control

c00Control:
    btfsc c00,7
	goto notEaten
    movwf c00		; fruit is eaten
    goto eaten
c01Control:
    btfsc c01,7
	goto notEaten
    movwf c01		; fruit is eaten
    goto eaten
c02Control:
    btfsc c02,7
	goto notEaten
    movwf c02		; fruit is eaten
    goto eaten
c03Control:
    btfsc c03,7
	goto notEaten
    movwf c03		; fruit is eaten
    goto eaten


c10Control:
    btfsc c10,7
	goto notEaten
    movwf c10		; fruit is eaten
    goto eaten
c11Control:
    btfsc c11,7
	goto notEaten
    movwf c11		; fruit is eaten
    goto eaten
c12Control:
    btfsc c12,7
	goto notEaten
    movwf c12		; fruit is eaten
    goto eaten
c13Control:
    btfsc c13,7
	goto notEaten
    movwf c13		; fruit is eaten
    goto eaten

c20Control:
    btfsc c20,7
	goto notEaten
    movwf c20		; fruit is eaten
    goto eaten
c21Control:
    btfsc c21,7
	goto notEaten
    movwf c21		; fruit is eaten
    goto eaten
c22Control:
    btfsc c22,7
	goto notEaten
    movwf c22		; fruit is eaten
    goto eaten
c23Control:
    btfsc c23,7
	goto notEaten
    movwf c23		; fruit is eaten
    goto eaten

c30Control:
    btfsc c30,7
	goto notEaten
    movwf c30		; fruit is eaten
    goto eaten
c31Control:
    btfsc c31,7
	goto notEaten
    movwf c31		; fruit is eaten
    goto eaten
c32Control:
    btfsc c32,7
	goto notEaten
    movwf c32		; fruit is eaten
    goto eaten
c33Control:
    btfsc c33,7
	goto notEaten
    movwf c33		; fruit is eaten
    goto eaten

eaten:
    call incScore
    call updateFruitLights
    return

notEaten:
    call updateFruitLights
    return

phase2
    call isGameOver
    call updateDisplayPhase2
    call updateCount
    call controlCounts
    call newFruit
    call toggleLight
    call move          
    call isEaten
    call reflectRemainingLives

    return

phase3init
    ; about interrupts
    clrf INTCON
    clrf INTCON2
    clrf T0CON          ; configuration of Timer-0. Not on yet
    bcf RCON, IPEN
    movlw b'00001000'
    movwf T0CON	
    bsf T0CON, TMR0ON	; Timer-0: on

    movlw b'00010000'
    movwf TRISA		    ; RA4: input
    movlw b'00001111'
    movwf ADCON1	    ; RA4: digital input


    ; display state
    movlw b'00000001'
    movwf displayState

    ; clear latches
    clrf LATA
    clrf LATB
    clrf LATC
    clrf LATD
    clrf LATE
    clrf LATF

    ; phaseInfo settings
    movlw b'00000100'
    movwf phaseInfo

    movlw d'0'
    cpfseq lives
        goto success
    goto unsuccess

    success:
        movlw d'5'
        movwf phase3success
        movlw b'10100000'	    ; global interrupt: 1, timer0 interrupt: 1
        movwf INTCON
        call updateDisplayPhase3
        return
    unsuccess:
        movlw d'10'
        movwf phase3success
        movlw b'10100000'	    ; global interrupt: 1, timer0 interrupt: 1
        movwf INTCON
        call updateDisplayPhase3
        return

; start: RA4 polling for phase1
isRA4PressedPhase3
    btfsc PORTA, 4
    call RA4pressedPhase3
    return

RA4pressedPhase3
    call waitRA4releasePhase3
    call delay256Phase3
    call phase1init
    return

waitRA4releasePhase3
    call delay256Phase3
    btfsc PORTA, 4
    bra waitRA4releasePhase3
    return
; end: RA4 polling for phase1

delay256Phase3
    movlw d'255'
    movwf delay256Var
    movwf delay256Var2

delay256Loop2Phase3:
    dcfsnz delay256Var2
    return

    ;call updateDisplayPhase3
    movwf delay256Var
delay256Loop1Phase3:
    decfsz delay256Var
    goto delay256Loop1Phase3

    goto delay256Loop2Phase3
    return

phase3
    call isRA4PressedPhase3
    return

main
    btfsc phaseInfo, 0
        call phase1
    btfsc phaseInfo, 1
        call phase2
    btfsc phaseInfo, 2
        call phase3
    goto main

end

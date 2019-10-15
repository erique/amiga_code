
ENABLE_KPRINTF

VNAME	MACRO
		dc.b	'sd'
	ENDM

VFULL	MACRO
		dc.b	'Replay SDCARD'
	ENDM

VERSION		EQU	1
REVISION	EQU	0

VSTR	MACRO
		dc.b	'1.0'
	ENDM

VDATE	MACRO
		dc.b	'01.10.2019'
		ENDM

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	incdir	"SYS:Code/NDK_3.9/Include/include_i/"

	include "exec/ables.i"
	include "exec/execbase.i"
	include "exec/errors.i"
	include "exec/io.i"
	include "exec/memory.i"

	include "dos/dosextens.i"

	include "exec/libraries.i"
	include "exec/devices.i"
	include	"exec/resident.i"
	include	"exec/initializers.i"

	include "hardware/intbits.i"

	include "libraries/expansion.i"
	include "libraries/configvars.i"

	include "utility/utility.i"
	include	"lvo/exec_lib.i"
	include "lvo/expansion_lib.i"
	include	"lvo/utility_lib.i"

	include "devices/timer.i"
	include "devices/trackdisk.i"
	include "devices/scsidisk.i"
	include "utility/tagitem.i"
	include "utility/hooks.i"

	include "kprintf.i"

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	IFD	ENABLE_KPRINTF

	kprintf	"%cc%c[2J",#$001b001b
	kprintf	"%c[0;32m",#$1b<<16
	kprintf	"%s",#IDString
	kprintf	"%c[0m",#$1b<<16

	kprintf "d0/a0 = %lx/%lx",d0,a0

	move.l	4.w,a1
	move.l	ThisTask(a1),a1
	move.l	pr_CLI(a1),d1
	tst.l	d1
	beq.b	.nocli

	lsl.l	#2,d1
	movea.l	d1,a1
	move.l  cli_CommandName(a1),d1
	lsl.l	#2,d1
	movea.l	d1,a1
	moveq.l	#0,d1
	move.b	(a1)+,d1
	clr.b	(a1,d1.w)

	kprintf "from cli %s %s",a1,a0

.nocli	moveq.l	#0,d0
	rts

	ELSE

	moveq.l	#-1,d0
	rts

	ENDC


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	STRUCTURE GlobalData,DD_SIZE

	; AmigaDOS device variables
		APTR	g_ExecBase
		APTR	g_SegList

		APTR	g_BoardAddr

		ULONG	g_MotorState

		UBYTE	g_CardType
		ULONG	g_NumBlocks
		ULONG	g_BytesPerBlock

		ALIGNLONG
		LABEL	GLOBALDATA_SIZE

	ENUM
		EITEM CARDTYPE_NONE
		EITEM CARDTYPE_MMC
		EITEM CARDTYPE_SD
		EITEM CARDTYPE_SDHC

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

VSTRING	MACRO
		VNAME
		dc.b	' '
		VSTR
		dc.b	' ('
		VDATE
		dc.b	') '
		VFULL
		dc.b	13,10,0
		even
	ENDM

VERSTAG	MACRO
		dc.b	0,'$VER: '
		VSTRING
		dc.b	0
		even
	ENDM

DEVNAME	MACRO
		VNAME
		dc.b	'.device',0
		even
	ENDM

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; %% AmigaDOS Device Driver
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		VERSTAG			; $VER: ...

RomTag:		dc.w	RTC_MATCHWORD	; RT_MATCHWORD
		dc.l	RomTag		; RT_MATCHTAG
		dc.l	end		; RT_ENDSKIP
		dc.b	RTF_AUTOINIT	; RT_FLAGS
		dc.b	VERSION		; RT_VERSION
		dc.b	NT_DEVICE	; RT_TYPE
		dc.b	0		; RT_PRI
		dc.l	DeviceName	; RT_NAME
		dc.l	IDString	; RT_IDSTRING
		dc.l	Init		; RT_INIT

DeviceName:	DEVNAME
IDString:	VSTRING

Init:		dc.l	GLOBALDATA_SIZE		; data space size
		dc.l	.funcTable		; pointer to function initializers
		dc.l	.dataTable		; pointer to data initializers
		dc.l	.initRoutine		; routine to run at startup

.funcTable:
		dc.w	-1
		dc.w	OpenDevice-.funcTable
		dc.w	CloseDevice-.funcTable
		dc.w	ExpungeDevice-.funcTable
		dc.w	Null-.funcTable
		dc.w	BeginIO-.funcTable
		dc.w	AbortIO-.funcTable
		dc.w	-1

.dataTable:
		INITBYTE	LN_TYPE,NT_DEVICE
		INITLONG	LN_NAME,DeviceName
		INITBYTE	LIB_FLAGS,LIBF_SUMUSED|LIBF_CHANGED
		INITWORD	LIB_VERSION,VERSION
		INITWORD	LIB_REVISION,REVISION
		INITLONG	LIB_IDSTRING,IDString
		dc.w	0

.initRoutine:	; ( device:d0, seglist:a0, execbase:a6 )
		kprintf	"%cc%c[2J",#$001b001b
		kprintf "CODE STARTS AT %lx - ENDS AT %lx - SIZE EQUALS %lx",#RomTag,#end,#(end-RomTag)

		movem.l	d1-a6,-(sp)

		kprintf	"initRoutine( device = %lx, seglist = %lx, execbase = %lx )",d0,a0,a6

;		; 020+ only
;		btst	#AFB_68020,AttnFlags+1(a6)
;		beq.b	.cpufail
;
;		; OS3.0+ only
;		cmp.w	#39,LIB_VERSION(a6)
;		blt	.osfail

		; Store constants
		move.l	a6,a1			; execbase in a1
		move.l	d0,a6			; device ptr in a6
		move.l	a1,g_ExecBase(a6)	; save execbase ptr
		move.l  a0,g_SegList(a6)	; save seglist ptr for Expunge

		; everything but d0/d1/a0/a1 must be preserved
		; device ptr = a6

;		bsr	SetupGlobalSANAII
;		beq	.sanafailed
;		bsr	SetupGlobalENC624
;		beq	.hwfailed

		move.l	a6,d0			; restore device ptr == return value
		kprintf	"   device driver initialized (d0 = %lx)",d0
.out		movem.l	(sp)+,d1-a6
		rts

.cpufail
		kprintf	"   requires 68020+"
		bra	.error

.osfail
		kprintf	"   requires V39+"
		bra.b	.error

.sanafailed
		kprintf	"   SANA-II failed"
		bra.b	.error

.hwfailed	;bsr	FreeSANAResources
		kprintf	"   hardware setup failed"

.error		bsr	FreeDevice
		moveq.l	#0,d0			; return NULL
		bra	.out

OpenDevice:	; ( unitnum:d0, flags:d1, iob:a1, device:a6 )
		kprintf	"OpenDevice( unit = %lx, flags = %lx, iob = %lx, device = %lx )",d0,d1,a1,a6

		addq.w	#1,LIB_OPENCNT(a6)	; take a temp reference

		kprintf	"Replay SPI/SDCARD Unit = %lx",d0
		tst.l	d0
		bne	.failed
		moveq.l	#1,d0
		move.l	d0,IO_UNIT(a1)

		bsr	GetBoardAddr
		kprintf	"Replay SPI/SDCARD BoardAddr = %lx",d0
		move.l	d0,g_BoardAddr(a6)
		beq	.failed

		bsr	Card_Init
		tst.b	g_CardType(a6)
		beq.b	.nocard

		bsr	Card_GetCapacity

.nocard
		clr.b	IO_ERROR(a1)
		move.b	#NT_REPLYMSG,LN_TYPE(a1)

		addq.w  #1,LIB_OPENCNT(a6)
		bclr    #LIBB_DELEXP,LIB_FLAGS(a6)
		moveq   #0,d0

.done		subq.w	#1,LIB_OPENCNT(a6)	; decrease temp reference cnt
		kprintf	"    OpenDevice returns %lx",d0
		rts

.failed		moveq.l	#IOERR_OPENFAIL,d0
		move.b	d0,IO_ERROR(a1)
		move.l	d0,IO_DEVICE(a1)
		kprintf	"    OpenDevice failed!"
		bra	.done

CloseDevice:	; ( iob:a1, device:a6 ) - returns seglist if unloading
	kprintf	"CloseDevice( iob = %lx, device = %lx )",a1,a6

		moveq.l	#0,d0
		move.w	LIB_OPENCNT(a6),d0
		kprintf	"    LIB_OPENCNT = %lx",d0

		tst.w	LIB_OPENCNT(a6)		; can this happen?
		beq.b	.done

		moveq.l	#-1,d1
		move.l	d1,IO_UNIT(a1)
		move.l	d1,IO_DEVICE(a1)

	;	bsr	CloseUnit

		subq.w  #1,LIB_OPENCNT(a6)
		bne.b   .keep

;		bsr	ReleaseHardware		; let go of the board

		btst	#LIBB_DELEXP,LIB_FLAGS(a6)
		beq.b	.keep

		bsr.b	ExpungeDevice		; returns seglist:d0

.done		kprintf	"    CloseDevice returns %lx",d0
		rts

.keep		kprintf	"    Device still in use"
		moveq.l	#0,d0
		bra.b	.done

ExpungeDevice:	; ( device: a6 ) - return seglist if refcount = 0
	kprintf	"ExpungeDevice( device = %lx )",a6

	tst.w	LIB_OPENCNT(a6)
	bne.w	.delayed

	;	bsr	ReleaseHardware		; let go of the board
	; bsr	FreeSANAResources	; free pools etc
	;bsr	FreeENC624Resources

	kprintf	"    Ejecting Device!"

	move.l	g_SegList(a6),-(sp)	; save seglist pointer

	move.l	a6,a1
	kprintf	"    removing device %lx",a1
	REMOVE (a1)

	move.l	a6,d0
	bsr.w	FreeDevice

	kprintf	"    %s expunged!",#DeviceName

	movem.l (sp)+,d0
	bra.b	.done

.delayed
	kprintf	"    open count not 0; delaying expunge"

	bset	#LIBB_DELEXP,LIB_FLAGS(a6)
	moveq.l	#0,d0
.done
 	kprintf	"    ExpungeDevice returns %lx",d0
 	rts


FreeDevice:	; ( device: d0 )
	kprintf	"    freeing device %lx",d0
	move.l  d0,a1
	moveq.l	#0,d0
	move.w	LIB_NEGSIZE(a6),d0
	sub.l	d0,a1                        ; calculate base of functions
	add.w	LIB_POSSIZE(a6),d0           ; calculate size of functions + data area
	movea.l	4.w,a6

	kprintf	"    free device memory %ld bytes",d0
	CALLLIB	_LVOFreeMem (a1,d0)

	rts
Null:
	kprintf	"Null()"
	moveq.l	#0,d0
	rts

COMMAND	MACRO
	dc.w	(\1)-(.jmptbl)
	ENDM

NSDCMD	MACRO
	dc.w	(\1)-(.nsdtbl),(\2)
	ENDM

	STRUCTURE NSDeviceQueryResult,0
		ULONG	DevQueryFormat		; /* this is type 0               */
		ULONG	SizeAvailable		; /* bytes available              */
		UWORD	DeviceType		; /* what the device does         */
		UWORD	DeviceSubType		; /* depends on the main type     */
		APTR	SupportedCommands	; /* 0 terminated list of cmd's   */
		LABEL	NSDeviceQueryResult_SIZEOF

NSDEVTYPE_TRACKDISK	EQU	5
TD_READ64		EQU	24
TD_WRITE64		EQU	25
TD_SEEK64		EQU	26
TD_FORMAT64		EQU	27
NSCMD_DEVICEQUERY	EQU	$4000
NSCMD_TD_READ64		EQU	$C000
NSCMD_TD_WRITE64	EQU	$C001
NSCMD_TD_SEEK64		EQU	$C002
NSCMD_TD_FORMAT64	EQU	$C003

BeginIO:	; ( iob: a1, device:a6 )
	move.l	#*,a0
	kprintf	"BeginIO( %lx, %lx ) @ %lx ( %lx )",a1,a6,a0,*

	moveq.l	#0,d1
	moveq.l	#0,d0
	move.b	IO_FLAGS(a1),d1
	move.w	IO_COMMAND(a1),d0

	kprintf	"    IO_DEVICE = %lx, IO_UNIT = %lx, IO_FLAGS = %lx, IO_COMMAND = %lx",IO_DEVICE(a1),IO_UNIT(a1),d1,d0

	move.b	#NT_MESSAGE,LN_TYPE(a1)
	clr.b	IO_ERROR(a1)

;	kprintf	"-->> break @ %lx <<--",#*

	cmpi.w	#(.endtbl-.jmptbl)/2,d0
	bhs.b	.nsdcmd

;	kprintf	"   -> about to jump to command handler %lx ...",d0

	add.w	d0,d0
	move.w	.jmptbl(pc,d0.w),d0
	beq	.nocmd

	jmp	.jmptbl(pc,d0.w)	

.nsdcmd

	kprintf	"NewStyleDevice"
;	bra	.nocmd

	lea	.nsdtbl(pc),a0
.next	move.l	(a0)+,d1
	beq.b	.nocmd
	cmp.w	d0,d1
	bne.b	.next

	swap	d1
	jmp	.nsdtbl(pc,d1.w)	

	cnop	0,4
.nsdtbl
	NSDCMD	CmdDeviceQuery,NSCMD_DEVICEQUERY
	NSDCMD	CmdRead64,NSCMD_TD_READ64
	NSDCMD	CmdWrite64,NSCMD_TD_WRITE64
	NSDCMD	CmdSeek64,NSCMD_TD_SEEK64
	NSDCMD	CmdFormat64,NSCMD_TD_FORMAT64
	dc.w	0,0

	cnop	0,4

.jmptbl	
	COMMAND	.nocmd			; 00 = CMD_INVALID		(invalid command)
	COMMAND	.nocmd			; 01 = CMD_RESET		(reset as if just inited)
	COMMAND	CmdRead			; 02 = CMD_READ			(standard read)
	COMMAND	CmdWrite		; 03 = CMD_WRITE		(standard write)
	COMMAND	CmdUpdate		; 04 = CMD_UPDATE		(write out all buffers)
	COMMAND	CmdClear		; 05 = CMD_CLEAR		(clear all buffers)
	COMMAND	CmdStop			; 06 = CMD_STOP			(hold current and queued)
	COMMAND	CmdStart		; 07 = CMD_START		(restart after stop)
	COMMAND	CmdFlush		; 08 = CMD_FLUSH		(abort entire queue)
	COMMAND	CmdMotor		; 09 = TD_MOTOR			(control the disk's motor)
	COMMAND	CmdSeek			; 0a = TD_SEEK			(explicit seek (for testing))
	COMMAND	CmdFormat		; 0b = TD_FORMAT		(format disk)
	COMMAND	CmdRemove		; 0c = TD_REMOVE		(notify when disk changes)
	COMMAND	CmdChangeNum		; 0d = TD_CHANGENUM		(number of disk changes)
	COMMAND	CmdChangeState		; 0e = TD_CHANGESTATE		(is there a disk in the drive?)
	COMMAND	CmdProtStatus		; 0f = TD_PROTSTATUS		(is the disk write protected?)
	COMMAND	CmdRawRead		; 10 = TD_RAWREAD		(read raw bits from the disk)
	COMMAND	CmdRawWrite		; 11 = TD_RAWWRITE		(write raw bits to the disk)
	COMMAND	CmdGetDriveType		; 12 = TD_GETDRIVETYPE		(get the type of the disk drive)
	COMMAND	CmdGetNumTracks		; 13 = TD_GETNUMTRACKS		(get the # of tracks on this disk)
	COMMAND	CmdAddChangeInt		; 14 = TD_ADDCHANGEINT		(TD_REMOVE done right)
	COMMAND	CmdRemChangeInt		; 15 = TD_REMCHANGEINT		(removes softint set by ADDCHANGEINT)
	COMMAND	CmdGetGeometry		; 16 = TD_GETGEOMETRY		(gets the disk geometry table)
	COMMAND	CmdEject		; 17 = TD_EJECT			(for those drives that support it)
	COMMAND	CmdRead64		; 18 = TD_READ64		(TrackDisk64 ; 64-bit byte offset - io_Offset = lo32bit, io_Actual = hi32bit)
	COMMAND	CmdWrite64		; 19 = TD_WRITE64		(TrackDisk64 ; 64-bit byte offset - io_Offset = lo32bit, io_Actual = hi32bit)
	COMMAND	CmdSeek64		; 1a = TD_SEEK64		(TrackDisk64 ; 64-bit byte offset - io_Offset = lo32bit, io_Actual = hi32bit)
	COMMAND	CmdFormat64		; 1b = TD_FORMAT64		(TrackDisk64 ; 64-bit byte offset - io_Offset = lo32bit, io_Actual = hi32bit)
	COMMAND	CmdSCSI			; 1c = HD_SCSICMD
.endtbl

.nocmd
	kprintf	"IO_ERROR = IOERR_NOCMD"

	move.b	#IOERR_NOCMD,IO_ERROR(a1)
	bra.w	TermIO


AbortIO:	; ( iob: a1, device:a6 )
	kprintf	"AbortIO( %lx, %lx)",a1,a6

		rts


TermIO:		; a1 : io request
;	kprintf	"TermIO %lx",a1

;	kprintf	"   -> about to reply to message ..."

;	move.b	#NT_REPLYMSG,LN_TYPE(a1)	; wait - is this kosher?
	btst	#IOB_QUICK,IO_FLAGS(a1)
	bne.b	.quick

;	kprintf	"   -> replying to message ..."

	; ReplyMsg(message:a1)
		LINKLIB	_LVOReplyMsg,$4.w

.quick
;	kprintf	"   -> done!"
	rts



; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
CmdDeviceQuery
		kprintf	"CmdDeviceQuery"

		move.l	IO_LENGTH(a1),d0
		kprintf	"IO_LENGTH = %lx ; IO_DATA = %lx",IO_LENGTH(a1),IO_DATA(a1)
		move.l	IO_DATA(a1),d0
		beq.b	.error
		movea.l	d0,a0
;		kprintf	"DevQueryFormat = %lx ; SizeAvailable = %lx",DevQueryFormat(a0),SizeAvailable(a0)
;		tst.l	DevQueryFormat(a0)	; "this is type 0"
;		bne.b	.error
;		tst.l	SizeAvailable(a0)
;		beq.b	.error
		clr.l	DevQueryFormat(a0)
		move.l	#NSDeviceQueryResult_SIZEOF,SizeAvailable(a0)
		move.w	#NSDEVTYPE_TRACKDISK,DeviceType(a0)
		clr.w	DeviceSubType(a0)
		move.l	#.supported,SupportedCommands(a0)
		move.l	#NSDeviceQueryResult_SIZEOF,IO_ACTUAL(a1)
		bra	TermIO

.error		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
.supported
		dc.w	CMD_READ
		dc.w	CMD_WRITE
		dc.w	CMD_UPDATE
		dc.w	CMD_CLEAR
		dc.w	CMD_STOP
		dc.w	CMD_START
		dc.w	CMD_FLUSH
		dc.w	TD_MOTOR
		dc.w	TD_SEEK
		dc.w	TD_FORMAT
		dc.w	TD_REMOVE
		dc.w	TD_CHANGENUM
		dc.w	TD_CHANGESTATE
		dc.w	TD_PROTSTATUS
		dc.w	TD_RAWREAD
		dc.w	TD_RAWWRITE
		dc.w	TD_GETDRIVETYPE
		dc.w	TD_GETNUMTRACKS
		dc.w	TD_ADDCHANGEINT
		dc.w	TD_REMCHANGEINT
		dc.w	TD_GETGEOMETRY
		dc.w	TD_EJECT
		dc.w	TD_READ64
		dc.w	TD_WRITE64
		dc.w	TD_SEEK64
		dc.w	TD_FORMAT64
;		dc.w	HD_SCSICMD
;		dc.w	NSCMD_DEVICEQUERY
;		dc.w	NSCMD_TD_READ64
;		dc.w	NSCMD_TD_WRITE64
;		dc.w	NSCMD_TD_SEEK64
;		dc.w	NSCMD_TD_FORMAT64
		dc.w	0

CmdRead
		kprintf	"CmdRead"
		clr.l	IO_ACTUAL(a1)
		bra	CmdRead64
CmdWrite
		kprintf	"CmdWrite"
		clr.l	IO_ACTUAL(a1)
		bra	CmdWrite64
CmdUpdate
		kprintf	"CmdUpdate"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdClear
		kprintf	"CmdClear"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdStop
		kprintf	"CmdStop"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdStart
		kprintf	"CmdStart"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdFlush
		kprintf	"CmdFlush"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdMotor
		kprintf	"CmdMotor %lx / %lx",g_MotorState(a6),IO_LENGTH(a1)
		move.l	g_MotorState(a6),IO_ACTUAL(a1)
		tst.l	IO_LENGTH(a1)
		beq.b	.done
		move.l	#1,g_MotorState(a6)
		bra.b	.done
.off		move.l	#0,g_MotorState(a6)
.done		bra	TermIO

CmdSeek
		kprintf	"CmdSeek"
		clr.l	IO_ACTUAL(a1)
		bra	CmdSeek64
CmdFormat
		kprintf	"CmdFormat"
		clr.l	IO_ACTUAL(a1)
		bra	CmdFormat64
CmdRemove
		kprintf	"CmdRemove"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdChangeNum
		kprintf	"CmdChangeNum"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdChangeState
		kprintf	"CmdChangeState"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdProtStatus
		kprintf	"CmdProtStatus"
		move.l	#0,IO_ACTUAL(a1)
		bra	TermIO
CmdRawRead
		kprintf	"CmdRawRead"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdRawWrite
		kprintf	"CmdRawWrite"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdGetDriveType
		kprintf	"CmdGetDriveType"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdGetNumTracks
		kprintf	"CmdGetNumTracks"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdAddChangeInt
		kprintf	"CmdAddChangeInt"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdRemChangeInt
		kprintf	"CmdRemChangeInt"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdGetGeometry
		kprintf	"CmdGetGeometry"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdEject
		kprintf	"CmdEject"
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdRead64
		kprintf	"CmdRead64 (%08lx:%08lx, %lx)",IO_ACTUAL(a1),IO_OFFSET(a1),IO_LENGTH(a1)

		move.l	IO_ACTUAL(a1),d1
		and.l	#(1<<9)-1,d1
		swap	d1
		lsl.l	#7,d1
		move.l	IO_OFFSET(a1),d0
		lsr.l	#8,d0
		lsr.l	#1,d0
		or.l	d1,d0
		move.l	IO_LENGTH(a1),d1
		lsr.l	#8,d1
		lsr.l	#1,d1
		move.l	IO_DATA(a1),a0
		bsr	Card_ReadM

		bra	TermIO
CmdWrite64
		kprintf	"CmdWrite64 (%08lx:%08lx, %lx)",IO_ACTUAL(a1),IO_OFFSET(a1),IO_LENGTH(a1)
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdSeek64
		kprintf	"CmdSeek64 (%08lx:%08lx, %lx)",IO_ACTUAL(a1),IO_OFFSET(a1),IO_LENGTH(a1)
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO
CmdFormat64
		kprintf	"CmdFormat64 (%08lx:%08lx, %lx)",IO_ACTUAL(a1),IO_OFFSET(a1),IO_LENGTH(a1)
		move.b	#IOERR_NOCMD,IO_ERROR(a1)
		bra	TermIO

;#define OPERATIONCODE_READ_FORMAT_CAPACITY 0x23
;#define OPERATIONCODE_READ_CAPACITY 0x25
;#define OPERATIONCODE_READ_10 0x28
;#define OPERATIONCODE_WRITE_10 0x2a
;#define OPERATIONCODE_PREVENT_ALLOW_REMOVAL 0x1e
;#define OPERATIONCODE_REQUEST_SENSE 0x03

OPERATIONCODE_TEST_UNIT_READY  = $00
OPERATIONCODE_READ_CAPACITY = $25
OPERATIONCODE_MODE_SENSE_6 = $1a
OPERATIONCODE_INQUIRY = $12

CmdSCSI		movem.l	d0-a6,-(sp)

		move.l	IO_DATA(a1),a0
		kprintf	"CmdSCSI (IO_LENGTH = %lx ; CMD = %lx ; LEN = %lx)",IO_LENGTH(a1),scsi_Command(a0),scsi_CmdLength(a0)

		cmp.l	#scsi_SIZEOF,IO_LENGTH(a1)
		bhs.b	.sizeok
		kprintf	"   ***** ILLEGAL SIZE = %lx",IO_LENGTH(a1)

.sizeok		move.l	IO_DATA(a1),a0
		move.l	scsi_Command(a0),d0
		beq.b	.badaddress
		cmp.l	#6,scsi_CmdLength(a0)
		blo	.badlength

		clr.l	scsi_Actual(a0)

		move.l	d0,a2
		moveq.l	#0,d0
		move.b	(a2),d0
		kprintf	"    SCSI = %lx",d0
		cmp.b	#OPERATIONCODE_READ_CAPACITY,d0
		beq	.readcapacity
		cmp.b	#OPERATIONCODE_MODE_SENSE_6,d0
		beq	.modesense6
		cmp.b	#OPERATIONCODE_INQUIRY,d0
		beq	.inquiry
		cmp.b	#OPERATIONCODE_TEST_UNIT_READY,d0
		beq	.done

		kprintf	"    Unknown SCSI command"

		move.b	#IOERR_NOCMD,IO_ERROR(a1)
.done		movem.l	(sp)+,d0-a6
		bra	TermIO

.badaddress	kprintf	"    IOERR_BADADDRESS"
		move.b	#IOERR_BADADDRESS,IO_ERROR(a1)
		bra	.done

.badlength	kprintf	"    IOERR_BADLENGTH"
		move.b	#IOERR_BADLENGTH,IO_ERROR(a1)
		bra	.done

.readcapacity	kprintf	"    OPERATIONCODE_READ_CAPACITY"
		tst.l	2(a2)
		bne.b	.nopmi
		btst	#0,8(a2)
		bne.b	.nopmi

		cmp.l	#4+4,scsi_Length(a0)
		blo	.badlength

		move.l	scsi_Data(a0),a3
		move.l	g_NumBlocks(a6),(a3)
		subq.l	#1,(a3)+
		move.l	g_BytesPerBlock(a6),(a3)

;		move.l	#4+4,IO_ACTUAL(a1)
		move.l	#4+4,scsi_Actual(a0)
		
		bra	.done

.nopmi		kprintf	"    PMI obsolete"
		move.b	#HFERR_BadStatus,IO_ERROR(a1)
		bra	.done

.modesense6	
		moveq.l	#0,d0
		move.w	2(a2),d0
		kprintf	"    OPERATIONCODE_MODE_SENSE_6 (%lx)",d0

		move.b	#HFERR_BadStatus,IO_ERROR(a1)
		bra	.done

.inquiry	kprintf	"    OPERATIONCODE_INQUIRY (size = %lx)",scsi_Length(a0)
		move.l	scsi_Length(a0),d0
		cmp.l	#.inquirysize,d0
		blo.b	.shortdata

		move.l	#.inquirysize,d0
		kprintf	"    enough space for %lx",d0

.shortdata	kprintf	"    copying $%lx bytes",d0
		move.l	scsi_Data(a0),a3
		lea	.inquirydata(pc),a4
		move.l	d0,scsi_Actual(a0)
		bra.b	.start
.copyinquiry	move.b	(a4)+,(a3)+
.start		dbf	d0,.copyinquiry

		bra	.done

.inquirydata
    ; byte 0
    dc.b $00           ; PERIPHERALQUALIFIER(3) | PERIPHERALDEVICETYPE(5)
    ; byte 1
    			; RMB(1) | reserved(7)
    dc.b $80           ; This is a REMOVABLE device
    ; byte 2
    dc.b $02           ; $02 = "Obsolete", $06 = The device complies to ANSI INCITS 513-2015 (SPC-4)
    ; byte 3
    			; obsolete(2) | NORMACA(1) | HISUP(1) | RESPONSEDATAFORMAT(4)
    dc.b $02           ; Response Data Format: SPC-2/SPC-3/SPC-4 (2)
    ; byte 4
    dc.b .inquirysize-5 ; The ADDITIONAL LENGTH field indicates the length in bytes of the remaining standard INQUIRY data
    ; byte 5
			; SCCS(1) | _ACC(1) | TPGS(2) | TPC(1) | reserved(2) | PROTECT(1)
    dc.b $00           ; is NOT supported
    ; byte 6
			; BQUE(1) | ENCSERV(1) | VS1(1) | MULTIP(1) | obsolete(4)
    dc.b $00           ; is NOT supported
    ; byte 7
    			; obsolete(6) | CMDQUE(1) | VS2(1)
    dc.b $00           ; is NOT supported
    ; byte 8-15
    dc.b 'A','r','c','a','d','e',' ',' '  ; Vendor ID
    ; byte 16-31
    dc.b 'F','P','G','A','A','r','c','a','d','e','R','e','p','l','a','y'  ; Product ID
    ; byte 32-35
    dc.b '1','.','0','0'                  ; Revision level
    ; byte 36-43
    dc.b '1','2','3','4','5','6','7','8'
.inquiryend:
.inquirysize = .inquiryend-.inquirydata
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;-----------------------------------------------------------

GetBoardAddr:	; ( device:a6 )
.VENDOR		= 5060	; Replay
.PRODUCT	= 28	; spi/sdcard
		movem.l	d1/a0/a1/a6,-(sp)
		moveq.l	#0,d0
		movea.l	g_ExecBase(a6),a6
		lea	.expansionName(pc),a1
		jsr	_LVOOpenLibrary(a6)
		tst.l	d0
		beq.b	.exit

		move.l	a6,-(sp)
		movea.l	d0,a6
		suba.l	a0,a0

.findNext	move.l	#.VENDOR,d0
		move.l	#.PRODUCT,d1
		jsr	_LVOFindConfigDev(a6)
		tst.l	d0
		beq.b	.noBoard
		move.l	d0,a0
		bclr	#CDB_CONFIGME,cd_Flags(a0)
		move.l	cd_BoardAddr(a0),d0

.noBoard	movea.l	a6,a1
		movea.l	(sp),a6
		move.l	d0,(sp)
		jsr	_LVOCloseLibrary(a6)
		move.l	(sp)+,d0

.exit		movem.l	(sp)+,d1/a0/a1/a6
		rts

.expansionName	EXPANSIONNAME


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SPI_STATUS_REG	= $0100
SPI_BYTE_REG	= $0102
SPI_CS		= $0104
SPI_CLK_CNT	= $0106

	BITDEF	STATUS,CDET,15
	BITDEF	STATUS,READY,7
	BITDEF	STATUS,CLKDIV6,6
	BITDEF	STATUS,CLKDIV5,5
	BITDEF	STATUS,CLKDIV4,4
	BITDEF	STATUS,CLKDIV3,3
	BITDEF	STATUS,CLKDIV2,2
	BITDEF	STATUS,CLKDIV1,1
	BITDEF	STATUS,CLKDIV0,0

SYS_MAIN_CLK	= 28367516	; 28.367516 MHz
SPI_MAIN_CLK	= (SYS_MAIN_CLK/2)
SPI_CLK_400K	= ((SPI_MAIN_CLK/400000)+1)
SPI_CLK_20M	= ((SPI_MAIN_CLK/20000000)+1)
SPI_CLK_25M	= ((SPI_MAIN_CLK/25000000))

CMD0  = $40        ; Resets the multimedia card
CMD1  = $41        ; Activates the card's initialization process
CMD2  = $42        ; --
CMD3  = $43        ; --
CMD4  = $44        ; --
CMD5  = $45        ; reseved
CMD6  = $46        ; reserved
CMD7  = $47        ; --
CMD8  = $48        ; reserved
CMD9  = $49        ; CSD : Ask the selected card to send its card specific data
CMD10 = $4a        ; CID : Ask the selected card to send its card identification
CMD11 = $4b        ; --
CMD12 = $4c        ; --
CMD13 = $4d        ; Ask the selected card to send its status register
CMD14 = $4e        ; --
CMD15 = $4f        ; --
CMD16 = $50        ; Select a block length (in bytes) for all following block commands (Read:between 1-512 and Write:only 512)
CMD17 = $51        ; Reads a block of the size selected by the SET_BLOCKLEN command, the start address and block length must be set so that the data transferred will not cross a physical block boundry
CMD18 = $52        ; --
CMD19 = $53        ; reserved
CMD20 = $54        ; --
CMD21 = $55        ; reserved
CMD22 = $56        ; reserved
CMD23 = $57        ; reserved
CMD24 = $58        ; Writes a block of the size selected by CMD16, the start address must be alligned on a sector boundry, the block length is always 512 bytes
CMD25 = $59        ; --
CMD26 = $5a        ; --
CMD27 = $5b        ; Programming of the programmable bits of the CSD
CMD28 = $5c        ; If the card has write protection features, this command sets the write protection bit of the addressed group. The porperties of the write protection are coded in the card specific data (WP_GRP_SIZE)
CMD29 = $5d        ; If the card has write protection features, this command clears the write protection bit of the addressed group
CMD30 = $5e        ; If the card has write protection features, this command asks the card to send the status of the write protection bits. 32 write protection bits (representing 32 write protect groups starting at the specific address) followed by 16 CRD bits are transferred in a payload format via the data line
CMD31 = $5f        ; reserved
CMD32 = $60        ; sets the address of the first sector of the erase group
CMD33 = $61        ; Sets the address of the last sector in a cont. range within the selected erase group, or the address of a single sector to be selected for erase
CMD34 = $62        ; Removes on previously selected sector from the erase selection
CMD35 = $63        ; Sets the address of the first erase group within a range to be selected for erase
CMD36 = $64        ; Sets the address of the last erase group within a continuos range to be selected for erase
CMD37 = $65        ; Removes one previously selected erase group from the erase selection
CMD38 = $66        ; Erases all previously selected sectors
CMD39 = $67        ; --
CMD40 = $68        ; --
CMD41 = $69        ; reserved
CMD42 = $6a        ; reserved
CMD43 = $6b        ; reserved
CMD44 = $6c        ; reserved
CMD45 = $6d        ; reserved
CMD46 = $6e        ; reserved
CMD47 = $6f        ; reserved
CMD48 = $70        ; reserved
CMD49 = $71        ; reserved
CMD50 = $72        ; reserved
CMD51 = $73        ; reserved
CMD52 = $74        ; reserved
CMD53 = $75        ; reserved
CMD54 = $76        ; reserved
CMD55 = $77        ; reserved
CMD56 = $78        ; reserved
CMD57 = $79        ; reserved
CMD58 = $7a        ; reserved
CMD59 = $7b        ; Turns the CRC option ON or OFF. A '1' in the CRC option bit will turn the option ON, a '0' will turn it OFF
CMD60 = $7c        ; --
CMD61 = $7d        ; --
CMD62 = $7e        ; --
CMD63 = $7f        ; --

rSPI_STATUS	MACRO
		move.l	d0,-(sp)
		moveq.l	#0,d0
		move.w	SPI_STATUS_REG(a5),d0
		kprintf	"SPI_STATUS_REG = %lx",d0
		move.l	(sp)+,d0
		ENDM

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;-----------------------------------------------------------
t dc.l	0
rSPI	MACRO	; a5 = SPI base
.waitrdy1\@	btst	#STATUSB_READY,SPI_STATUS_REG+1(a5)
		beq	.waitrdy1\@
		move.l	\1,t
		move.w	\1,SPI_BYTE_REG(a5)	; only lower byte used

.waitrdy2\@	btst	#STATUSB_READY,SPI_STATUS_REG+1(a5)
		beq	.waitrdy2\@

		move.w	SPI_BYTE_REG(a5),\1	; upper byte always 0s

		and.l	#$ff,t
		and.l	#$ff,\1
;		kprintf	"    SPI_BYTE_REG = %02lx => %02lx",t,\1
	ENDM


rMC_CRC	MACRO	; \1 = data, \2 = crc, \3 loop reg, a5 = SPI base
;		kprintf	"    SPI_BYTE_REG = %lx",\1
		move.w	\1,SPI_BYTE_REG(a5)	; only lower byte used

		moveq.l	#8-1,\3
.crcloop\@	lsl.b	\2
		bpl.b	.crcpos\@
		eor.b	#$09,\2
.crcpos\@	lsl.b	\1
		bcc.b	.dataok\@
		eor.b	#$09,\2
.dataok\@	dbf	\3,.crcloop\@

.waitrdy2\@	btst	#STATUSB_READY,SPI_STATUS_REG+1(a5)
		beq	.waitrdy2\@

		move.w	SPI_BYTE_REG(a5),\1	; upper byte always 0s
	ENDM


ATTEMPTS = 100

MMC_Command	; d0 = cmd, d1 = arg
		movem.l	d2/d3/d7,-(sp)
		kprintf "MMC_Command %02lx %08lx",d0,d1
		moveq.l	#ATTEMPTS-1,d7
		moveq.l	#-1,d2
.getResp1	rSPI	d2
		cmp.b	#$ff,d2
		dbeq	d7,.getResp1

		moveq.l	#0,d2		; crc
		moveq.l	#0,d3		; out reg
		rMC_CRC	d0,d2,d7	; CMD $xx

		rol.l	#8,d1
		move.b	d1,d3
		rMC_CRC	d3,d2,d7	; $xx......

		rol.l	#8,d1
		move.b	d1,d3
		rMC_CRC	d3,d2,d7	; $..xx....

		rol.l	#8,d1
		move.b	d1,d3
		rMC_CRC	d3,d2,d7	; $....xx..

		rol.l	#8,d1
		move.b	d1,d3
		rMC_CRC	d3,d2,d7	; $......xx

		lsl.b	d2
		addq.l	#1,d2
;		kprintf	"CRC = %lx",d2
		rSPI	d2

		moveq.l	#ATTEMPTS-1,d7
		moveq.l	#-1,d0
.getResp2	rSPI	d0
		cmp.b	#$ff,d0
		dbne	d7,.getResp2

		kprintf	"response = %lx",d0

		movem.l	(sp)+,d2/d3/d7
		rts

MMC_CMD	MACRO
		move.l	#\1,d0
		move.l	#\2,d1
		bsr	MMC_Command
	ENDM


SPI_SetClkDiv	MACRO
		move.w	#\1,SPI_STATUS_REG(a5)
		ENDM

SPI_EnableCard	MACRO
		move.w	#0,SPI_CS(a5)
		ENDM

SPI_DisableCard	MACRO
		move.w	#1,SPI_CS(a5)
		ENDM

Wait25us:	move.l	d0,-(sp)
		moveq.l	#25,d0
.wait		tst.b	$bfe001		; approx 1us
		dbf	d0,.wait
		move.l	(sp)+,d0
		rts

Wait250us:	move.w	d0,-(sp)
		move.w	#250,d0
.wait		tst.b	$bfe001		; approx 1us
		dbf	d0,.wait
		move.w	(sp)+,d0
		rts

Wait20ms:	move.w	d0,-(sp)
		move.w	#20000,d0
.wait		tst.b	$bfe001		; approx 1us
		dbf	d0,.wait
		move.w	(sp)+,d0
		rts


Card_Init
		movem.l	d0-a6,-(sp)

		move.l	g_BoardAddr(a6),a5

		move.b	#CARDTYPE_NONE,g_CardType(a6)
		btst	#STATUSB_CDET,SPI_STATUS_REG(a5)
		bne	.cardok
		kprintf	"SPI:Card_Init: Card NOT Detected"
		bra	.out

.cardok		kprintf	"SPI:Card_Init: Card Detected"
		moveq.l	#3-1,d7
.tryinit	; disableIRQ ?
		bsr	Card_TryInit
		; enableIRQ ?
		cmp.b	#CARDTYPE_NONE,g_CardType(a6)
		bne.b	.out
		kprintf	"SPI:Card_Init:Attempt %ld failed!",d7
	rept 5
		bsr	Wait20ms
	endr
		dbf	d7,.tryinit

		kprintf	"SPI:Card_Init: Failed!"

.out		movem.l	(sp)+,d0-a6
		rts



CARD_INIT_TIMEOUT = 1000

Card_TryInit
		movem.l	d0-a6,-(sp)
		kprintf	"SPI:Card_TryInit"

		SPI_SetClkDiv	SPI_CLK_400K

		rSPI_STATUS

		SPI_DisableCard

		moveq.l	#10-1,d7
.reset		moveq.l	#-1,d6
		rSPI	d6
		dbf	d7,.reset

		bsr	Wait20ms

		SPI_EnableCard

	; if (MMC_Command(CMD0, 0) == 0x01) {      /*CMD0: Reset all cards to IDLE state*/

		MMC_CMD	CMD0,$0
		cmp.b	#1,d0
		bne	.done

	; if (MMC_Command(CMD8, 0x1AA) == 0x01) { // check if the card can operate with 2.7-3.6V power

		MMC_CMD	CMD8,$01AA
		cmp.b	#1,d0
		bne	.not3v3

		lea	.ocr(pc),a0
		bsr	.readocr

	; if (ocr[2] == 0x01 && ocr[3] == 0xAA) {

		lea	.ocr+2(pc),a0
		cmp.w	#$01AA,(a0)
		bne	.not3v3

		kprintf	"SPI:Card_Init SDHC card detected"

	; SDHC init

		move.w	#CARD_INIT_TIMEOUT,d7
		bra	.trysdhc

	; // now we must wait until CMD41 returns 0 (or timeout elapses)
.retrysdhc
	; // CMD55 must precede any ACMD command
		MMC_CMD	CMD55,$0
		cmp.b	#1,d0
		bne	.trysdhc

		MMC_CMD	CMD41,(1<<30)	; // ACMD41 with HCS bit
		cmp.b	#0,d0
		bne	.trysdhc

		MMC_CMD	CMD58,$0
		cmp.b	#0,d0
		bne	.cmd58failed

	; // check CCS (Card Capacity Status) bit in the OCR
		lea	.ocr(pc),a0
		bsr	.readocr
	; // if CCS set then the card is SDHC compatible
		lea	.ocr(pc),a0
		btst	#30,(a0)
		bne	.sdhc

		kprintf	"    CARDTYPE_SD"
		move.b	#CARDTYPE_SD,g_CardType(a6)
		bra	.done

.sdhc		kprintf	"    CARDTYPE_SDHC"
		move.b	#CARDTYPE_SDHC,g_CardType(a6)
		bra	.done

.trysdhc	dbf	d7,.retrysdhc

		kprintf	"SPI:Card_Init SDHC card initialization timed out!"
		bra	.done

.cmd58failed	kprintf	"SPI:Card_Init CMD58 (READ_OCR) failed!"
		bra	.done

	; -------------------------

.not3v3		move.w	#CARD_INIT_TIMEOUT,d7
		bra.b	.trysd

	; // if it's not an SDHC card perhaps it is an SD card?
.retrysd	MMC_CMD	CMD55,$0
		cmp.b	#1,d0
		bne	.trymmc
	; // CMD55 accepted so it's an SD card (or Kingston 128 MB MMC)
		MMC_CMD	CMD41,$0
		cmp.b	#0,d0
		bne	.trysd

		kprintf	"    CARDTYPE_SD"
		move.b	#CARDTYPE_SD,g_CardType(a6)
		bra.b	.cmd16

.trysd		dbf	d7,.retrysd

		kprintf	"SPI:Card_Init SD card initialization timed out!"
		bra	.done

	; MMC init
.retrymmc	MMC_CMD	CMD1,$0
		cmp.b	#0,d0
		bne	.trymmc

		kprintf	"    CARDTYPE_MMC"
		move.b	#CARDTYPE_MMC,g_CardType(a6)

.cmd16		MMC_CMD	CMD16,512
		cmp.b	#0,d0
		beq	.done

		kprintf	"SPI:Card_Init CMD16 (SET_BLOCKLEN) failed!"
		bra.b	.done

.trymmc		dbf	d7,.retrymmc

		kprintf	"SPI:Card_Init MMC card initialization timed out!"

	; -------------------------

.done		SPI_DisableCard

		move.b	g_CardType(a6),d0
		cmp.b	#CARDTYPE_NONE,d0
		bhi.b	.gotcard

		kprintf	"SPI:Card_Init No memory card detected!"
		bra.b	.out

.gotcard	cmp.b	#CARDTYPE_MMC,d0
		bhi.b	.hifreq

		kprintf	"   SPI:Clock = 20MHz"
		SPI_SetClkDiv	SPI_CLK_20M
		bra.b	.out

.hifreq		cmp.b	#CARDTYPE_SDHC,d0
		bhi.b	.error

		kprintf	"   SPI:Clock = 25MHz"
		SPI_SetClkDiv	SPI_CLK_25M
		bra.b	.out

.error		kprintf	"SPI:Card_Init Unknown MMC/SDCARD!"

.out		movem.l	(sp)+,d0-a6
		rts

.readocr	moveq.l	#4-1,d7
.ocrloop	moveq.l	#-1,d0
		rSPI	d0
		move.b	d0,(a0)+
		dbf	d7,.ocrloop
		rts

.ocr		ds.b	4

Card_GetCapacity
		movem.l	d0-a6,-(sp)
		move.l	g_BoardAddr(a6),a5

		kprintf	"SPI:Card_GetCapacity()"

		SPI_EnableCard

		MMC_CMD	CMD9,$0

		moveq.l	#10-1,d7
.wait		moveq.l	#-1,d6
		rSPI	d6
		cmp.b	#$fe,d6
		beq	.readcsd
		bsr	Wait250us
		dbf	d7,.wait

		kprintf	"SPI:Card_CMD9 (SEND_CSD) timeout!"
		SPI_DisableCard
		bra	.done

.readcsd	lea	.csd(pc),a0
		moveq.l	#16-1,d7
.csdloop	moveq.l	#-1,d0
		rSPI	d0
		and.l	#$ff,d0
		kprintf "CSD = %02lx",d0
		move.b	d0,(a0)+
		dbf	d7,.csdloop

		moveq.l	#-1,d0
		rSPI	d0	; // CRC hi
		moveq.l	#-1,d0
		rSPI	d0	; // CRC lo

		SPI_DisableCard

		lea	.csd(pc),a0
		moveq.l	#0,d0
		move.b	0(a0),d0
		lsr.b	#6,d0

		kprintf	"csdVersion = %ld",d0

		cmp.b	#0,d0
		bne	.not10
		kprintf	"CARD:CSD Version 1.0 / Standard Capacity"

		; READ_BL_LEN
		move.b	5(a0),d0
		and.b	#$0f,d0

		; C_SIZE
		move.l	5(a0),d1
		lsr.l	#6,d1
		and.l	#(1<<12)-1,d1

		; C_SIZE_MULT
		moveq.l	#0,d2
		move.w	9(a0),d2
		lsr.w	#7,d2
		and.w	#(1<<3)-1,d2

		kprintf	"READ_BL_LEN = %ld ; C_SIZE = %lu ; C_SIZE_MULT = %ld",d0,d1,d2

	; uint64_t sectorSize = (1 << READ_BL_LEN);
	; uint64_t sectorCount = (C_SIZE + 1) * (1 << (C_SIZE_MULT + 2));

		; sectorSize
		moveq.l	#1,d3
		lsl.l	d0,d3

		; sectorCount
		addq.l	#1,d1
		addq.l	#2,d2
		moveq.l	#1,d4
		lsl.l	d2,d4
		mulu	d1,d4

		kprintf	"sectorSize = %ld ; sectorCount = %ld",d3,d4
		bra	.done

.not10		cmp.b	#1,d0
		bne	.not20
		kprintf	"CARD:CSD Version 2.0 / High Capacity"


		; READ_BL_LEN
		move.b	5(a0),d0
		and.b	#$0f,d0

		; C_SIZE
		move.l	6(a0),d1
		and.l	#(1<<22)-1,d1

		kprintf	"READ_BL_LEN = %ld ; C_SIZE = %ld",d0,d1

	; uint64_t sectorSize = (1 << READ_BL_LEN);
	; uint64_t sectorCount = (C_SIZE + 1) * 1024;

		; sectorSize
		moveq.l	#1,d3
		lsl.l	d0,d3

		; sectorCount
		addq.l	#1,d1
		move.l	d1,d4
		lsl.l	#8,d4
		lsl.l	#2,d4

		kprintf	"sectorSize = %ld ; sectorCount = %ld",d3,d4

		bra	.done

.not20		kprintf	"CARD:Unknown CSD version!"

.done		mulu.l	d3,d5:d4
		kprintf	"    capacity in bytes = %08lx.%08lx",d5,d4
		and.l	#(1<<9)-1,d5
		swap	d5
		lsl.l	#7,d5
		lsr.l	#8,d4
		lsr.l	d4
		or.l	d4,d5
		kprintf	"    capacity in blocks = %08lx (512 bytes)",d5
		move.l	d5,g_NumBlocks(a6)
		move.l	#(1<<9),g_BytesPerBlock(a6)

		movem.l	(sp)+,d0-a6
		rts
.csd		ds.b	16


Card_ReadM	; (d0 = sector offset, d1 = sector length, a0 = buffer, a6 = device)
		movem.l	d0-a6,-(sp)
		move.l	g_BoardAddr(a6),a5
		kprintf	"SPI:Card_ReadM(%08lx, %lu, %lu, %08lx)",a0,d0,d1,a6

		SPI_EnableCard

		move.l	d1,d7	; sector length
		move.l	d0,d1	; sector offet

		cmp.b	#CARDTYPE_SDHC,g_CardType(a6)
		beq.b	.sdhc
		lsl.l	#8,d1
		add.l	d1,d1
.sdhc
	
	; if (numSectors == 1) {
		subq.l	#1,d7
		bmi	.done
		beq.b	.single

	; if (MMC_Command(CMD18, sector)) {
		moveq.l	#CMD18,d0
		bra.b	.setoffset

	; if (MMC_Command(CMD17, sector)) {
.single		moveq.l	#CMD17,d0

.setoffset	bsr	MMC_Command
		tst.b	d0
		bne	.offsetfailed

	; while (sectorCount--) {
		moveq.l	#0,d2
.sectorLoop

	; while (rSPI(0xFF) != 0xFE)
		move.w	#1000-1,d6
.waittoken	moveq.l	#-1,d0
		rSPI	d0
		cmp.b	#$fe,d0
		beq	.gottoken
		dbf	d6,.waittoken

		kprintf	"SPI:Card_ReadM - no data token! (lba=%lu)",d1
		bra	.done
.gottoken

;         // read sector bytes
;         for (uint32_t offset = 0; offset < 512; offset++) {
;             pBuffer[offset] = rSPI(0xff);
;         }

		move.w	#512-1,d6
.byteLoop	moveq.l	#-1,d0
		rSPI	d0
		move.b	d0,(a0)+
		dbf	d6,.byteLoop

		moveq.l	#-1,d0
		rSPI	d0	; // CRC hi
		moveq.l	#-1,d0
		rSPI	d0	; // CRC lo

		addq.l	#1,d2
		dbf	d7,.sectorLoop

	; if (numSectors != 1) {
		kprintf	"sectors read : %lx",d2
		subq.l	#1,d2
		bne.b	.mmc_cmd12
.done
		SPI_DisableCard

		movem.l	(sp)+,d0-a6
		rts

.offsetfailed	kprintf	"SPI:Card_ReadM CMD17/8 - invalid response 0x%02lX (lba=%lu)",d0,d1
		bra.b	.done

.mmc_cmd12
	; WORKAROUND for no compliance card (Atmel Internal ref. !MMC7 !SD19):
	; The errors on this command must be ignored
	; and one retry can be necessary in SPI mode for no compliance card.
		MMC_CMD	CMD12,$0
		moveq.l	#-1,d0
		rSPI	d0
		tst.b	d0
		beq.b	.cmd12ok
		MMC_CMD	CMD12,$0
		moveq.l	#-1,d0
		rSPI	d0
.cmd12ok	

	; while (rSPI(0xFF) != 0xFF) {// wait until the card is not busy
		moveq.l	#ATTEMPTS-1,d7
		moveq.l	#-1,d0
.busy		rSPI	d0
		cmp.b	#$ff,d0
		beq	.done
		dbf	d7,.busy

		kprintf "SPI:Card_CMD12 (STOP) timeout!"
		bra	.done


Card_WriteM	; (d0 = sector offset, d1 = sector length, a0 = buffer, a6 = device)
		movem.l	d0-a6,-(sp)
		move.l	g_BoardAddr(a6),a5
		kprintf	"SPI:Card_WriteM(%08lx, %lu, %lu, %08lx)",a0,d0,d1,a6

		SPI_EnableCard

		move.l	d0,d6	; start offset
		move.l	d0,d7
		add.l	d1,d7	; end offset

.sectorLoop
		move.l	d6,d1
		cmp.b	#CARDTYPE_SDHC,g_CardType(a6)
		beq.b	.sdhc
		lsl.l	#8,d1
		add.l	d1,d1
.sdhc

		moveq.l	#CMD24,d0
		bsr	MMC_Command
		tst.b	d0
		bne	.offsetfailed

		moveq.l	#-1,d0
		rSPI	d0	; // one byte gap
		moveq.l	#-2,d0
		rSPI	d0	; // send Data Token

		moveq.l	#0,d0
		move.w	#512-1,d5
.byteLoop	move.b	(a0)+,d0
		rSPI	d0
		dbf	d5,.byteLoop

		moveq.l	#-1,d0
		rSPI	d0	; // CRC hi
		moveq.l	#-1,d0
		rSPI	d0	; // CRC lo

		moveq.l	#-1,d0
		rSPI	d0	; // read packet response
;         // Status codes
;         // 010 = Data accepted
;         // 101 = Data rejected due to CRC error
;         // 110 = Data rejected due to write error
		and.l	#$1f,d0
		cmp.b	#$05,d0
		bne	.invalidstatus

	; while (rSPI(0xFF) == 0x00) {
		move.w	#10000-1,d5
.waitwrite	moveq.l	#-1,d0
		rSPI	d0
		tst.b	d0
		bne	.writedone
		dbf	d5,.waitwrite

		kprintf	"SPI:Card_WriteM - busy write timeout! (lba=%lu)",d1
		bra	.done
.writedone

		addq.l	#1,d6
		cmp.l	d6,d7
		bne	.sectorLoop
.done
		SPI_DisableCard

		movem.l	(sp)+,d0-a6
		rts

.offsetfailed	kprintf	"SPI:Card_WriteM CMD24 - invalid response 0x%02lX (lba=%lu)",d0,d1
		bra	.done
.invalidstatus	kprintf	"SPI:Card_WriteM - invalid status 0x%02lX (lba=%lu)",d0,d1
		bra	.done



end:


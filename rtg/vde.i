;
; WWW.FPGAArcade.COM
;
; REPLAY Retro Gaming Platform
; No Emulation No Compromise
;
; Replay.card - P96 RTG driver for the REPLAY Amiga core
; Copyright (C) FPGAArcade community
;
; Contributors : Jakub Bednarski, Mike Johnson, Jim Drew, Erik Hemming, Nicolas Hamel
;
; This software is licensed under LPGLv2.1 ; see LICENSE file
;
;

VDE_DisplayStatus    = $100
VDE_DisplaySwitch    = $106
VDE_InterruptRequest = $108
VDE_InterruptEnable  = $10A
VDE_DisplayFormat    = $110
VDE_DisplayFlags     = $112
VDE_ClockDivider     = $114
VDE_DisplayBase      = $120
VDE_BytesPerLine     = $12A
VDE_Modulo           = $12E
VDE_HorTotal         = $200
VDE_HorDisplayEnd    = $202
VDE_HorSyncStart     = $204
VDE_HorSyncEnd       = $206
VDE_VerTotal         = $208
VDE_VerDisplayEnd    = $20A
VDE_VerSyncStart     = $20C
VDE_VerSyncEnd       = $20E
VDE_ColourPalette    = $400

VDE_SpriteControl    = $2000
VDE_SpriteXPos       = $2004
VDE_SpriteYPos       = $2006
VDE_SpriteColours    = $2400
VDE_SpriteImage      = $2800

VBE_SRCPTR = $1000
VBE_DSTPTR = $100C
VBE_SRCMOD = $1010
VBE_DSTMOD = $101C
VBE_SIZEX  = $1020
VBE_SIZEY  = $1022
VBE_CONTROL = $1030
VBE_STATUS  = $1040

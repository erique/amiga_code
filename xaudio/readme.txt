replay.audio 0.3� - 060db support

Added support for new AddReplayMem and 060db

replay.audio 0.2� - somewhat more tested

This driver is purely a simple wrapper over the Replay's xaudio 16bit stereo audio-out sink.
It does support mono by duplicating the pre-mixed audio data, but will otherwise not try
to do any fancy conversion (also no hifi support).

It has been moderately tested (with basic success) with 

* DigiBooster Pro 2.21
* HippoPlayer 2.45
* EaglePlayer 2.05
* Play16 1.10

(using ahi.device 4.18)

This software is licensed under LPGLv2.1 ; see LICENSE file

~erique

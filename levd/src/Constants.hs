-- Constants.hs
-- Static constants for use in program

module Constants where

-- Usb identification
krakenX61Vendor  = 0x2433
krakenX61Product = 0xb200

-- Device specific usb command codes
krakenFanCode   = 0x12
krakenPumpCode  = 0x13
krakenColorCode = 0x10

-- Device specific usb stuff?
krakenInit  = 0x0002
krakenBegin = 0x0001

-- Absolute path to CPU temperature file
cpuTemperatureFilePath = "/sys/class/thermal/thermal_zone0/temp";

-- Absolute path to levd config file
levdConfigurationFilePath  = "/etc/leviathan/levd.cfg";

-- The devices default color
krakenDefaultColor = [
  krakenColorCode, 0xff, 0xff, 0xff,
  0x00, 0x00, 0x00, 0xff, 0x00, 0x00,
  0x3c, 0x01, 0x01, 0x01, 0x00, 0x00,
  0x01, 0x00, 0x01]

{-|
11 - interval
12 - interval
13 - enabled
14 - altBit
15 - blinking
-}




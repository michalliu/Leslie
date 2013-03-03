# GET/SET

 SET server:name "fido"
 
 GET server:name
 
  SET connections 10
  INCR connections => 11
  INCR connections => 12
  DEL connections
  INCR connections => 1

module XMWM.DBus.Interfaces (
  propertiesInterface,
  getMethod,
  getAllMethod,
  setMethod,
  busObjectPath,
  busInterface,
  busServiceName,
  listNamesMethod,
) where

import DBus (InterfaceName, MemberName, ObjectPath, BusName)

-- See documentation of standard interfaces at:
-- - https://pythonhosted.org/txdbus/dbus_overview.html
-- - https://dbus.freedesktop.org/doc/dbus-specification.html

-- Standard interfaces: Properties.

propertiesInterface :: InterfaceName
propertiesInterface = "org.freedesktop.DBus.Properties"

getMethod :: MemberName
getMethod = "Get"

getAllMethod :: MemberName
getAllMethod = "GetAll"

setMethod :: MemberName
setMethod = "Set"

-- Special administrative bus services.

busObjectPath :: ObjectPath
busObjectPath = "/org/freedesktop/DBus"

busInterface :: InterfaceName
busInterface = "org.freedesktop.DBus"

busServiceName :: BusName
busServiceName = "org.freedesktop.DBus"

listNamesMethod :: MemberName
listNamesMethod = "ListNames"

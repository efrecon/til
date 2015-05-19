Dim WSHShell

Set WSHShell = WScript.CreateObject("WScript.Shell")
Set WshSysEnv = WshShell.Environment("PROCESS")
Set myShortcut = WSHShell.CreateShortcut(Wscript.Arguments.Item(0))
WScript.Echo myShortcut.TargetPath

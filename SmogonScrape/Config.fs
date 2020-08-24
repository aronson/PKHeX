module Config
open FSharp.Configuration

// Let the type provider do its work
type ConfigFile = IniFile<"FilePathConfigs.ini">

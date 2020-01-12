module Config
open FSharp.Configuration

// Let the type provider do it's work
type ConfigFile = IniFile<"FilePathConfigs.ini">

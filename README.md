# Plugin Development Guide

## Overview

Plugins extend the host application by executing custom logic in response to Minecraft commands. A plugin is a VB.NET class that implements `IPlugin` and is decorated with `PluginMetadataAttribute`. When a player issues a command in Minecraft, the host looks up the corresponding plugin by name and calls its `Execute` method.

---

## First Run

On first launch, if the application does not find the required resources, it creates them and exits with a setup message: 

| Resource | Location | Description |
|----------|----------|-------------|
| Configuration file | `commands.ini` | Command mappings and settings |
| Plugins directory | `plugins/` | Location for plugin assemblies |

Both are created in the application's base directory. 

After first run: 

1. Edit `commands.ini` to configure command mappings and settings
2. Place your compiled plugin `.dll` files in the `plugins/` directory
3. Restart the application

---

## Plugin Location

Place your compiled plugin assembly (`.dll` file) in the `plugins/` directory located in the application's base directory: 

```
ApplicationDirectory/
├── Application.exe
├── commands.ini
└── plugins/
    ├── MyPlugin.dll
    └── AnotherPlugin.dll
```

The host scans this directory on startup and loads all assemblies containing valid `IPlugin` implementations.

---

## Writing a Plugin

### 1. Create the Class File

Create a new VB.NET class library project.  Import the plugin API namespace:

```vb
Imports Current. PluginApi
```

### 2. Add the Metadata Attribute

Every plugin requires the `PluginMetadata` attribute:

```vb
<PluginMetadata("My Plugin", "1.0", "Author", "Description of what the plugin does.")>
Public Class MyPlugin
```

| Parameter | Type | Description |
|-----------|------|-------------|
| `name` | String | Unique plugin name.  Used for command mapping.  Case-sensitive. |
| `version` | String | Version identifier |
| `author` | String | Plugin author |
| `description` | String | Brief summary of functionality |

> **Important:** The `name` parameter must exactly match the plugin name specified in `commands.ini`. A mismatch will result in the command being ignored.

### 3. Implement IPlugin

Your class must implement the `IPlugin` interface:

```vb
Public Class MyPlugin
    Implements IPlugin

    Private _api As ICurrentApi

    Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
        _api = api

        ' Plugin logic here
    End Sub
End Class
```

### 4. Complete Example

```vb
Imports Current. PluginApi

<PluginMetadata("Hello World", "1.0", "Developer", "Logs a message when triggered.")>
Public Class HelloWorldPlugin
    Implements IPlugin

    Private _api As ICurrentApi

    Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
        _api = api

        Console.WriteLine("Hello from the plugin!")
    End Sub
End Class
```

### 5. Build and Deploy

1. Build your class library project to produce a `.dll` file
2. Copy the `.dll` to the `plugins/` directory
3. Restart the host application

---

## Registering a Command

Plugins are triggered by commands defined in the `[Commands]` section of `commands.ini`.

### Format

Each command mapping consists of two consecutive lines: 

```ini
command string
Plugin Name
```

- **Line 1:** The command string as received from Minecraft
- **Line 2:** The plugin name (must match `PluginMetadata.Name` exactly)

### Example

To trigger the `Hello World` plugin with the command `say hello`:

```ini
[Commands]
say hello
Hello World
```

When a player types `say hello` in Minecraft, the host executes the plugin named `Hello World`.

### Comments

Lines beginning with `;` or `#` are treated as comments: 

```ini
[Commands]
; This is a comment
say hello
Hello World
```

### Built-in Commands

The host reserves certain keywords for built-in functionality:

| Keyword | Description |
|---------|-------------|
| `_builtin_list_plugins` | Lists all loaded plugins |
| `_builtin_list_margins` | Lists all defined margins |

Example: 

```ini
[Commands]
list plugins
_builtin_list_plugins

list margins
_builtin_list_margins
```

---

## API Reference

### IPlugin

```vb
Public Interface IPlugin
    Sub Execute(api As ICurrentApi)
End Interface
```

| Member | Description |
|--------|-------------|
| `Execute(api As ICurrentApi)` | Called by the host when the mapped command is received |

### PluginMetadataAttribute

```vb
<AttributeUsage(AttributeTargets.Class, AllowMultiple:=False)>
Public Class PluginMetadataAttribute
    Inherits Attribute

    Public Property Name As String
    Public Property Version As String
    Public Property Author As String
    Public Property Description As String

    Public Sub New(name As String, version As String, author As String, description As String)
End Class
```

| Parameter | Type | Description |
|-----------|------|-------------|
| `name` | String | Unique plugin name. Used for command mapping.  |
| `version` | String | Version identifier |
| `author` | String | Plugin author |
| `description` | String | Brief summary of functionality |

---

## Execution Details

### Threading

Plugins are executed on background threads.  The host calls `Execute` on a dedicated thread for each plugin invocation.  Long-running operations will not block the main application. 

If your plugin performs continuous work (animation loops, polling, etc.), implement appropriate sleep intervals and provide a mechanism to stop cleanly.

### Storing the API Reference

The `api` object passed to `Execute` provides access to host functionality.  If you need to use it outside of `Execute` (in helper methods, background threads, event handlers, etc.), store it in a class field: 

```vb
Private _api As ICurrentApi

Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
    _api = api

    ' _api is now available to other methods in this class
End Sub
```

The `api` reference remains valid for the lifetime of your plugin's execution.

### Console Output

Plugins can write to the console using `Console.WriteLine()`. When a plugin is loaded and executed, the host outputs: 

```
Loaded plugin: 'Plugin Name' v1.0 by Author: Description
```

---

## Configuration File Reference

The `commands.ini` file supports multiple sections.  Sections use `[SectionName]` headers.  Key-value pairs use `Key=Value` format.

### [Commands]

Maps Minecraft commands to plugin names. Two-line format per mapping. 

```ini
[Commands]
my command
My Plugin Name
```

### [CubeCenter]

Cube center coordinates (integer values):

```ini
[CubeCenter]
CenterX=-250
CenterY=73
CenterZ=-78
```

### [Tuning]

Runtime parameters (integer values):

```ini
[Tuning]
RequiredStationaryFrames=5
ScoreDecayPerFrame=1
ScoreBumpOnBlock=8
```

### [predefined margin sets]

Custom margin set definitions. 

```ini
[predefined margin sets]
; Your margin sets here
```

---

## Troubleshooting

### "Plugin 'X' not found"

The plugin name in `commands.ini` does not match any loaded plugin's `PluginMetadata.Name`. Verify: 

- The `.dll` file is in the `plugins/` directory
- The `name` parameter in `PluginMetadata` matches exactly (case-sensitive)
- The plugin class implements `IPlugin`
- The plugin class has the `PluginMetadata` attribute

### "No plugins found"

No valid plugin assemblies were found in the `plugins/` directory. Verify:

- The `plugins/` directory exists
- Your `.dll` files are in the directory
- Each plugin class implements `IPlugin` and has `PluginMetadata`

### "No plugins loaded"

Same as above.  The host scans `plugins/*. dll` for types implementing `IPlugin` with `PluginMetadata`.

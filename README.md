
**Installation**

&nbsp;&nbsp;**Prerequisites**:<br>
CommandHelper/MethodScript<br>
https://methodscript.com/docs/3.3.5//Download.html<br>
build (Recommended)

```
mkdir mc
mc/buildtools.exe
java -jar spigot.jar
/stop
mc/plugins/commandhelper-(Recommended).jar
java -jar spigot.jar
/stop
```

&nbsp;&nbsp;**Add these commands to `aliases.msa`**:<br>
mc/plugins/CommandHelper/aliases.msa<br>
https://github.com/nasuno/Holodeck/blob/main/CommandHelper/aliases.msa


&nbsp;&nbsp;**Add this to `main.ms`**:<br>
mc/plugins/CommandHelper/main.ms<br>
&nbsp;Two locations where "# Change me" occurs (without quotes), substitute username with your own.<br>
https://github.com/nasuno/Holodeck/blob/main/CommandHelper/main.ms<br>
You don't need **bind(block_place** but I can't live without it.


&nbsp;&nbsp;**And finally `auto_include.ms`**:<br>
mc/plugins/CommandHelper/auto_include.ms<br>
&nbsp;One location where "# Change me" occurs, substitute username with your own.<br>
https://github.com/nasuno/Holodeck/blob/main/CommandHelper/auto_include.ms

&nbsp;&nbsp;**Resource packs** (recomended)<br>
https://github.com/nasuno/Holodeck/tree/main/resourcepacks<br>
&nbsp;&nbsp;**Pre Minecraft 1.21.6:**<br>
`Holo_Wool-Glowstone`<br>
&nbsp;&nbsp;**Post Minecraft 1.21.6:**<br>
`Holo_Wool-Glowstone`<br>
`no-fog-without-optifine-1-21-6-e1750`





## First Run

On first launch, if the application does not find the required resources, it creates them and exits with a setup message: 

 Resource           |  Location      | Description 
--------------------|----------------|-------------
 Configuration file | `commands.ini` | Command mappings and settings 
 Plugins directory  | `plugins/`     | Location for plugin assemblies 

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
├── commands. ini
└── plugins/
    ├── MyPlugin.dll
    └── AnotherPlugin.dll
```

## Writing a Plugin

### 1. Create the Class File

Create a new VB.NET class library project.  Import the plugin API namespace:

```vb
Imports Current.PluginApi
```

### 2. Add the Metadata Attribute

Every plugin requires the `PluginMetadata` attribute placed above the class declaration:

```vb
<PluginMetadata("My Plugin", "1.0", "Author", "Description of what the plugin does.")>
Public Class MyPlugin
```

  Parameter    | Type   | Description 
---------------|--------|-------------
 `name`        | String | Unique plugin name.  Used for command mapping.  Case-sensitive. 
 `version`     | String | Version identifier 
 `author`      | String | Plugin author 
 `description` | String | Brief summary of functionality 

### 3. Implement IPlugin

Your class must implement the `IPlugin` interface:

```vb
    Implements IPlugin

    Private _api As ICurrentApi

    Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
        _api = api

        ' Some plugin logic
    End Sub
End Class
```

### 4. Complete Example

```vb
Imports Current.PluginApi

<PluginMetadata("My Plugin", "1.0", "Author", "Description of what the plugin does.")>
Public Class MyPlugin
    Implements IPlugin

    Private _api As ICurrentApi

    Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
        _api = api

        ' Some plugin logic
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

To trigger the `My Plugin` plugin with the command `zone test`:

```ini
[Commands]
plugin test
Spatial Zone Mouse Test
```

When a player types `plugin test` in Minecraft, the host executes the plugin named `My Plugin`.

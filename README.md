Creating a Plugin
Overview

Plugins extend the host application by executing custom logic in response to Minecraft commands. A plugin is a VB.NET class that implements IPlugin and is decorated with PluginMetadataAttribute. When a player issues a command in Minecraft, the host looks up the corresponding plugin by name and calls its Execute method.
Writing a Plugin
1. Create the Class File

Create a new VB.NET class file. Import the plugin API namespace at the top:
vb

Imports Current.PluginApi

2. Add the Metadata Attribute

Every plugin requires the PluginMetadata attribute. This tells the host how to identify and describe your plugin.
vb

<PluginMetadata("My Plugin", "1.0", "Author", "Description of what the plugin does.")>
Public Class MyPlugin

The parameters are:
Parameter	Description
name	The name used to map commands to this plugin. Must be unique.
version	Version string for your plugin.
author	Your name or identifier.
description	A brief explanation of the plugin's purpose.
3. Implement IPlugin

Your class must implement the IPlugin interface, which requires a single method:
vb

Public Class MyPlugin
    Implements IPlugin

    Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
        ' Plugin logic here
    End Sub
End Class

The api parameter provides access to host functionality. If you need to use it outside of Execute (in helper methods, background threads, event handlers, etc.), store it in a field:
vb

Private _api As ICurrentApi

Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
    _api = api

    ' Now _api is available throughout the class
End Sub

4. Complete Example
vb

Imports Current. PluginApi

<PluginMetadata("Hello World", "1.0", "Developer", "Logs a message when triggered.")>
Public Class HelloWorldPlugin
    Implements IPlugin

    Private _api As ICurrentApi

    Public Sub Execute(api As ICurrentApi) Implements IPlugin. Execute
        _api = api

        Console.WriteLine("Hello from the plugin!")
    End Sub
End Class

Registering a Command

Plugins are triggered by commands defined in the application's INI file. Open the INI file and locate the [Commands] section.

Each command mapping consists of two consecutive lines:

    The command string (as received from Minecraft)
    The plugin name (must match the name parameter in PluginMetadata exactly)

Example

To trigger the plugin above with the command say hello:
INI

[Commands]
say hello
Hello World

When a player types say hello in Minecraft, the host executes the plugin named Hello World.
Built-in Commands

The host reserves certain keywords for built-in functionality:
Keyword	Description
_builtin_list_plugins	Lists all available plugins.
_builtin_list_margins	Lists all defined margins.

Example:
INI

[Commands]
list plugins
_builtin_list_plugins

list margins
_builtin_list_margins

API Reference
IPlugin
vb

Public Interface IPlugin
    Sub Execute(api As ICurrentApi)
End Interface

Member	Description
Execute(api As ICurrentApi)	Called by the host when the mapped command is received.
PluginMetadataAttribute
vb

<PluginMetadata(name, version, author, description)>

Parameter	Type	Description
name	String	Unique plugin name. Used for command mapping.
version	String	Version identifier.
author	String	Plugin author.
description	String	Brief summary of functionality.
Addendum
Plugin Naming

The name in PluginMetadata is case-sensitive and must exactly match the name specified in the INI file. A mismatch will result in the command being ignored.
Storing the API Reference

The api object passed to Execute is your interface to the host. It is valid for the lifetime of your plugin's execution. If your plugin spawns background threads or registers callbacks, store api in a class field so those contexts can access host functionality.
Execution Context

Plugins are executed on a background thread by the host. Long-running operations will not block the main application. If your plugin performs continuous work (animation loops, polling, etc.), implement appropriate sleep intervals and provide a mechanism to stop cleanly when appropriate.

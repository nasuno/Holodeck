How To Create a Plugin
Step 1: Create Your Plugin Class File

Create a new VB.NET class file in your project. The filename can be anything, but should reflect your plugin's purpose (e.g., MyAwesomePlugin.vb).
Step 2: Add the Required Import

At the top of your file, import the plugin API namespace:
vb

Imports Current.PluginApi

This gives you access to IPlugin, ICurrentApi, and PluginMetadataAttribute.
Step 3: Add the Metadata Attribute

Decorate your class with the PluginMetadata attribute:
vb

<PluginMetadata("My Awesome Plugin", "1.0", "YourName", "Does something awesome.")>
Public Class MyAwesomePlugin

Important: The first parameter (name) is how the host identifies your plugin. You will use this exact string in the INI file to trigger execution. Spelling and capitalization must match exactly.
Metadata Parameters
Parameter	What to Write
name	A unique, descriptive name. This is your plugin's identity.
version	Your versioning scheme (e.g., "1.0", "2.1.3").
author	Your name or handle.
description	A sentence explaining what the plugin does.
Step 4: Implement the IPlugin Interface

Your class must implement IPlugin, which requires a single method:
vb

Public Class MyAwesomePlugin
    Implements IPlugin

    Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
        ' Your code runs here
    End Sub
End Class

The host calls Execute when your mapped command is received. The api parameter is your gateway to all host functionality.
Step 5: Store the API Reference

If your plugin does anything beyond a single immediate action—background threads, event handlers, helper methods—store the API reference in a field:
vb

Private _api As ICurrentApi

Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
    _api = api

    ' Now _api is available throughout your class
End Sub

Why this matters: The api parameter only exists within the Execute method scope. If you spawn a thread or register a callback, those will not have access to api unless you store it first.
Step 6: Register Your Command in the INI File

Open your application's INI file and locate the [Commands] section. Add two lines:
INI

[Commands]
your command here
My Awesome Plugin

    Line 1: The command string the host will receive from Minecraft.
    Line 2: Your plugin's name from the metadata attribute. Must match exactly.

Example Mappings
INI

[Commands]
spawn particles
Particle Spawner Plugin

toggle overlay
Debug Overlay

reset world
World Reset Tool

Each pair maps one command to one plugin.
Complete Example
The Plugin File
vb

Imports Current.PluginApi

<PluginMetadata("Hello World Plugin", "1.0", "DevName", "Prints a greeting when triggered.")>
Public Class HelloWorldPlugin
    Implements IPlugin

    Private _api As ICurrentApi

    Public Sub Execute(api As ICurrentApi) Implements IPlugin. Execute
        _api = api

        Console.WriteLine("Hello from the plugin!")
    End Sub
End Class

The INI Entry
INI

[Commands]
say hello
Hello World Plugin

What Happens

    User sends say hello from Minecraft.
    Host receives the command and looks it up in [Commands].
    Host finds Hello World Plugin and locates the class with matching PluginMetadata. Name.
    Host calls Execute(api) on that class.
    Console prints Hello from the plugin!.

Addendum
A. Built-in Commands

The host reserves certain keywords for internal functions. Do not use these as plugin names:
Keyword	Function
_builtin_list_plugins	Lists all available plugins
_builtin_list_margins	Lists all defined margins

Example INI usage:
INI

[Commands]
list plugins
_builtin_list_plugins

list margins
_builtin_list_margins

B. Plugin Execution Context

When Execute is called:

    It runs on a background thread (via RunPluginByNameOnThread).
    The api object is shared; avoid blocking operations that could stall other systems.
    Exceptions inside Execute should be handled gracefully to prevent crashes.

C. Naming Considerations
Do	Don't
Use clear, unique names	Use generic names like "Plugin" or "Test"
Include purpose in name	Rely on description alone for identification
Keep consistent capitalization	Mix cases between metadata and INI
D. Common Patterns

Animation/Continuous Loop:
vb

Private _running As Boolean = False
Private _thread As Thread

Public Sub Execute(api As ICurrentApi) Implements IPlugin. Execute
    _api = api

    If _thread IsNot Nothing AndAlso _thread.IsAlive Then Return

    _running = True
    _thread = New Thread(AddressOf RunLoop)
    _thread.IsBackground = True
    _thread. Start()
End Sub

Private Sub RunLoop()
    Do While _running
        ' Per-frame logic here
        Thread.Sleep(16)
    Loop
End Sub

Public Sub StopPlugin()
    _running = False
End Sub

One-Shot Action:
vb

Public Sub Execute(api As ICurrentApi) Implements IPlugin.Execute
    _api = api
    DoSomethingOnce()
End Sub

Private Sub DoSomethingOnce()
    ' Immediate action, then done
End Sub

E. Troubleshooting
Symptom	Likely Cause
Command does nothing	Plugin name in INI doesn't match PluginMetadata.Name exactly
Plugin not found	Class missing Implements IPlugin or PluginMetadata attribute
api is Nothing in helper method	Forgot to store _api = api in Execute
Plugin runs twice	No guard against re-entry (check if thread already running)

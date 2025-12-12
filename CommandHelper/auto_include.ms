


proc _exampleI() {    
    export('mccommand.idemp', 'clear')
    export('mouse.left.idemp', 'clear')
    export('mouse.right.idemp', 'clear')
    export('mccommand.text', 'dummy data')
    export('mouse.left.click', 'dummy data')
    export('mouse.right.click', 'dummy data')
    set_interval(50, 0, closure(
        _get_frame()
    ))
}


proc _single_frame() {
    _get_frame()
}



proc _get_frame() {
    @url = 'http://127.0.0.1:8080/data'
    @isMe = pinfo()
    @thirdString = @isMe[1]
    
    @observerLocation = concat(@thirdString[0],',',@thirdString[1],',',@thirdString[2])
    @possibleLocationLookedAt = pcursor('nasuno') # Change me
    @observedLocation = concat(@possibleLocationLookedAt[0],',',@possibleLocationLookedAt[1],',',@possibleLocationLookedAt[2])

    @latest_chat        = import('mccommand.text')
    @latest_chat_idemp  = import('mccommand.idemp')
    @mouse_right_coords = import('mouse.right.click')
    @mouse_right_idemp  = import('mouse.right.idemp')
    @mouse_left_coords  = import('mouse.left.click')
    @mouse_left_idemp   = import('mouse.left.idemp')
    

    @data = array(
        'observerLocation': @observerLocation,
        'observedLocation': @observedLocation,
        'mccommand': @latest_chat,
        'mccommandidemp': @latest_chat_idemp,
        'rightclickcoord': @mouse_right_coords,
        'rightlickidemp': @mouse_right_idemp,
        'leftclickcoord': @mouse_left_coords,
        'leftlickidemp': @mouse_left_idemp
    )

    http_request(@url, array(
        method: 'POST',
        headers: array(
            'Content-Type': 'application/json'
        ),
        params: json_encode(@data),
        success: closure(@response){
            @responseBody = @response['body']
            #msg('[DEBUG] Got response: ' . @responseBody)
            if(string_ends_with(@responseBody, 'END')) {
                assign(@splitStringArray, split('%', @responseBody))
                @nearingIndexString = array_get(@splitStringArray[0])
                assign(@stripped, replace(@nearingIndexString, "BEGIN", ""))
                assign(@parts, split("!", @stripped))

                // --- MC Command ack
                @test = array_get(@parts[0])
                if((@test == 'clear') or(@test == 'dummy data')) {                    
                    } else {
                        if(@test == @latest_chat_idemp) {
                            #msg('[DEBUG] Acked mccommand.idemp=' . @test . ', clearing')
                            export('mccommand.idemp', 'clear')
                            export('mccommand.text', 'dummy data')
                        }
                    }   


                
                // --- Mouse right ack
                @test1 = array_get(@parts[1])
                if((@test1 == 'clear') or(@test1 == 'dummy data')) {                    
                    } else {
                        if(@test1 == @mouse_right_idemp) {
                            #msg('[DEBUG] Acked mouse.right.idemp=' . @test1 . ', clearing')
                            export('mouse.right.idemp', 'clear')
                            export('mouse.right.click', 'dummy data')
                        }
                    }   

                
                // --- Mouse left ack
                @test2 = array_get(@parts[2])
                if((@test2 == 'clear') or(@test2 == 'dummy data')) {                    
                    } else {
                        if(@test2 == @mouse_left_idemp) {
                            #msg('[DEBUG] Acked mouse.left.idemp=' . @test2 . ', clearing')
                            export('mouse.left.idemp', 'clear')
                            export('mouse.left.click', 'dummy data')
                        }
                    }



                

                // --- (Optional: further response handling)
                @stripENDfromThis = array_get(@splitStringArray[1])
                assign(@lastSplitStringArray, split('BEGIN', @nearingIndexString))
                @newArray = split('END', @stripENDfromThis)
                @INDEXstring = array_get(@lastSplitStringArray[1])
                @COORDINATESstring = array_get(@newArray[0])
                _splitToBlackAndWhite(@COORDINATESstring)
            }
        },
        error: closure(@error){
            msg('[DEBUG] HTTP Error: '.@error)
        }
    ))
}



proc _colorblocks(@longstring, @wooltype) {
    assign(@splitstring, split(";", @longstring))
    @locations = array()
    foreach(@splitstring, @i) {
        @locations[] = _get_location_array(@i)
    }
    _batch_set_blocks(@locations, @wooltype)
}

proc _splitToBlackAndWhite(@theString) {
    #@theString = replace(@theString, "BEGIN", "")
    #@theString = replace(@theString, "END", "")

    assign(@splitstring, split("#", @theString))
    @segments = array()
    foreach(@splitstring, @part) {
        if(trim(@part) != "") {
            @segments[] = @part
        }
    }

    # Color mapping, index matches color order
    _colorblocks(@segments[0],  'glowstone')       # Black    I swapped these
    _colorblocks(@segments[1],  'black_wool')        # White  for a white background
    _colorblocks(@segments[2],  'yellow_wool')      # Yellow
    _colorblocks(@segments[3],  'white_wool')       # WhiteDim
    _colorblocks(@segments[4],  'light_gray_wool')  # GreyLight
    _colorblocks(@segments[5],  'gray_wool')        # Grey
    _colorblocks(@segments[6],  'brown_wool')       # Brown
    _colorblocks(@segments[7],  'red_wool')         # Red
    _colorblocks(@segments[8],  'orange_wool')      # Orange
    _colorblocks(@segments[9],  'lime_wool')        # Lime
    _colorblocks(@segments[10], 'green_wool')       # Green
    _colorblocks(@segments[11], 'cyan_wool')        # Cyan
    _colorblocks(@segments[12], 'light_blue_wool')  # BlueLight
    _colorblocks(@segments[13], 'blue_wool')        # Blue
    _colorblocks(@segments[14], 'purple_wool')      # Purple
    _colorblocks(@segments[15], 'magenta_wool')     # Magenta
    _colorblocks(@segments[16], 'pink_wool')        # Pink
}
proc _get_location_array(@coordString) {
    @splitstring = split(",", @coordString)
    return(array(@splitstring[0], @splitstring[1], @splitstring[2], 'world'))
}

proc _batch_set_blocks(@locations, @blockType) {
    // Disable physics updates
    @physics = false

    // Set blocks without physics updates
    foreach(@locations, @loc) {
        set_block(@loc, @blockType, @physics)
    }

    // Manually trigger block updates for the affected locations
    #foreach(@locations, @loc) {
    #    set_block(@loc, @blockType, true)
    #}
}











# Places all 6 planes of a cube using hardcoded corners and block type.
proc _place_cube_planes() {
    @blockType = 'stone'

    # ---- Hardcoded corners (edit these for your cube shape) ----
    @A = array(-373, 237, 45)
    @B = array(-127, 237, 45)
    @C = array(-127, 71, 45)
    @D = array(-373, 71, 45)
    @E = array(-373, 237, -201)
    @F = array(-127, 237, -201)
    @G = array(-127, 71, -201)
    @H = array(-373, 71, -201)
    #     A----B
    #    /|   /|
    #   D----C |
    #   | E--|-F
    #   |/   |/
    #   H----G

    # --- Planes as pairs of corners (no duplication, very readable) ---
    # Each face is defined by two diagonally opposite corners
    # (shares two coords with neighbors, as in a cube net)

    @planes = array(
        array(@A, @C), # Front  (z = 45)
        array(@B, @G), # Right  (x = -127)
        array(@G, @E), # Back   (z = -201)
        array(@A, @H), # Left   (x = -373)
        array(@E, @B), # Top    (y = 237)
        array(@D, @G)  # Bottom (y = 71)
    )

    # -- Place all planes using a single inner loop --
    foreach(@planes, @plane) {
        @c1 = @plane[0]
        @c2 = @plane[1]

        # Extract coordinates
        @x1 = @c1[0]; @y1 = @c1[1]; @z1 = @c1[2]
        @x2 = @c2[0]; @y2 = @c2[1]; @z2 = @c2[2]

        @minX = min(@x1, @x2); @maxX = max(@x1, @x2)
        @minY = min(@y1, @y2); @maxY = max(@y1, @y2)
        @minZ = min(@z1, @z2); @maxZ = max(@z1, @z2)

        # Determine the constant coordinate (plane orientation)
        if(@minX == @maxX) {
            for(@y = @minY, @y <= @maxY, @y++) {
                for(@z = @minZ, @z <= @maxZ, @z++) {
                    set_block(array(@minX, @y, @z, 'world'), @blockType, false)
                }
            }
        } else if(@minY == @maxY) {
            for(@x = @minX, @x <= @maxX, @x++) {
                for(@z = @minZ, @z <= @maxZ, @z++) {
                    set_block(array(@x, @minY, @z, 'world'), @blockType, false)
                }
            }
        } else if(@minZ == @maxZ) {
            for(@x = @minX, @x <= @maxX, @x++) {
                for(@y = @minY, @y <= @maxY, @y++) {
                    set_block(array(@x, @y, @minZ, 'world'), @blockType, false)
                }
            }
        } else {
            msg('Error: Plane definition does not form a rectangle!')
        }
    }
}




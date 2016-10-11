module Grid exposing (..)

import Char
import String
import Array exposing (Array)
import List.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { rows: Int
    , columns: Int
    , chars: Array (Array Char)
    , settings: Settings
    }

type alias Settings =
    { optimize: Bool
    , fontSize: Int
    , textWidth: Float
    , textHeight: Float
    , compactPath: Bool
    }

default: Settings
default =
    { optimize = True
    , fontSize = 14
    , textWidth = 8
    , textHeight = 16
    , compactPath = True
    }

init: String -> Model
init input =
    let
        lines = String.lines input
        chars = lines
                |> List.map (
                    \l -> l 
                        |> String.trimRight
                        |> String.toList
                        |> Array.fromList
                )
                |> Array.fromList

        max = chars 
                |> Array.map (\l -> Array.length l)
                |> Array.toList
                |> List.maximum
    in
        { rows = List.length lines
        , columns = Maybe.withDefault 0 max
        , chars = chars
        , settings = default
        }

type alias Point =
    { x: Float
    , y: Float
    }

type alias Loc =
    { x: Int
    , y: Int
    }

top: Loc -> Loc
top loc =
    {loc | y = loc.y - 1}

bottom: Loc -> Loc
bottom loc =
    {loc | y = loc.y + 1}

left: Loc -> Loc
left loc =
    {loc | x = loc.x - 1}

right: Loc -> Loc
right loc =
    { loc | x = loc.x + 1}

topLeft: Loc -> Loc
topLeft loc =
    top loc |> left

topRight loc = 
    top loc |> right

bottomLeft loc =
    bottom loc |> left

bottomRight loc = 
    bottom loc |> right




get: Loc -> Model -> Maybe Char
get loc model =
    let 
        line = Array.get loc.y model.chars
    in
        case line of
            Just l ->
                Array.get loc.x l
            Nothing ->
                Nothing

isVertical: Char -> Bool
isVertical ch =
    ch == '|'

isHorizontal: Char -> Bool
isHorizontal ch =
    ch == '-'

isVerticalDashed: Char -> Bool
isVerticalDashed ch =
    ch == ':'

isHorizontalDashed: Char -> Bool
isHorizontalDashed ch =
    ch == '='

isLowHorizontal: Char -> Bool
isLowHorizontal ch =
    ch == '_'

isSlantRight: Char -> Bool
isSlantRight ch =
    ch == '/'

isSlantLeft: Char -> Bool
isSlantLeft ch =
    ch == '\\'

    

isChar: Loc -> (Char -> Bool) -> Model -> Bool
isChar loc f model =
    case get loc model of
        Just ch ->
            f ch
        Nothing ->
            False

type Element
    = Line Point Point Stroke Marker 
    | Arc Point Point Float Bool
    | Text Loc String
    | Path Point Point String Stroke

type Marker 
    = Arrow
    | None

type Stroke
    = Solid
    | Dashed

getElements: Loc -> Model -> Maybe (List Element)
getElements loc model =
    let 
        textWidth = model.settings.textWidth
        textHeight = model.settings.textHeight

        measurex = textWidth * toFloat loc.x
        measurey = textHeight * toFloat loc.y

        ah = 0
        bh = textWidth / 4
        ch = textWidth / 2
        dh = textWidth * 3 / 4
        eh = textWidth

        av = 0
        bv = textHeight / 4
        cv = textHeight / 2
        dv = textHeight * 3 / 4
        ev = textHeight


        ax = measurex + ah
        bx = measurex + bh
        cx = measurex + ch
        dx = measurex + dh
        ex = measurex + eh

        ay = measurey + av
        by = measurey + bv
        cy = measurey + cv
        dy = measurey + dv
        ey = measurey + ev

        cxcy = Point cx cy
        cxay = Point cx ay
        cxey = Point cx ey
        axcy = Point ax cy
        excy = Point ex cy
        axey = Point ax ey
        exey = Point ex ey
        exay = Point ex ay
        axay = Point ax ay

        vertical = Line cxay cxey Solid None
        horizontal = Line axcy excy Solid None
        lowHorizontal = Line axey exey Solid None
        slantLeft = Line axay exey Solid None
        slantRight = Line axey exay Solid None

        matchList: List (Bool, List Element)
        matchList =
            [
            {--
                    |
            --}
             (isChar loc isVertical model
             , [vertical]  
             )
            {--
                     -
             --}
            ,(isChar loc isHorizontal model
             , [horizontal]
             )
            {--
                     _
             --}
            ,(isChar loc isLowHorizontal model
             , [lowHorizontal]
             )
            {--
                     /
             --}
            ,(isChar loc isSlantRight model
             , [slantRight]
             )
            {--
                     \
             --}
            ,(isChar loc isSlantLeft model
             , [slantLeft]
             )
            ]

        cond_elm = 
            matchList
            |> List.Extra.find ( \(cond, _) -> cond)
    in
        case cond_elm of
            Just (cond, elm) ->
                Just elm
            Nothing ->
                Nothing --TODO: add text element here


getAllElements: Model -> List (Loc, List Element)
getAllElements model =
    let 
        loc_elems: Array (List (Maybe (Loc, List Element)))
        loc_elems = 
            model.chars
                |> Array.indexedMap
                    (\y line ->
                        let 
                            per_lines: Array (Maybe (Loc, List Element)) 
                            per_lines =
                            line
                                |> Array.indexedMap
                                    (\x char ->
                                        let loc = Loc x y
                                            elems = getElements loc model
                                        in
                                            case elems of
                                                Just elems -> Just (loc, elems)
                                                Nothing -> Nothing
                                    )
                        in
                            Array.toList per_lines
                    )
    in
        Array.toList loc_elems
            |> List.concat
            |> List.filterMap (\a -> a)
 
elementToSvg: Element -> Svg a
elementToSvg elem =
    case elem of
        Line s e stroke feature ->
            line
                [x1 <| toString s.x
                ,y1 <| toString s.y
                ,x2 <| toString e.x
                ,y2 <| toString e.y
                ]
                []
        _ -> Debug.crash "not yet"
            

getSvg: Model -> Svg a
getSvg model =
    let
        fontSize = model.settings.fontSize
        gwidth = model.settings.textWidth * toFloat model.columns
        gheight = model.settings.textHeight * toFloat model.rows
        
        locElements: List (Loc, List Element)
        locElements = getAllElements model 
        components: List (List Element)
        components =
            locElements
                |>List.map
                    (\(loc,elm) ->
                        elm
                    )
        allElements = List.concat components
        svgElements =
            allElements
                |>List.map
                    (\elm -> elementToSvg elm)
    in
    svg [height <| toString gheight
        ,width <| toString gwidth
        ,Svg.Attributes.style ("font-size:"++toString fontSize++"px;font-family:monospace")
        ]
        (Svg.style [] [Svg.text svgStyle]
        :: 
        svgElements
        )

svgStyle =
    """    
    line, path {
      stroke: black;
      stroke-width: 1;
    }
    """

toSvg: String -> Svg a
toSvg input =
    init input
        |> getSvg
    

module Feliz.Molstar

open Fable.Core
open Fable.Core.JsInterop
open Feliz
open Browser.Types
open Browser

[<Erase>]
type molstar =
    static member inline useInterface (useInterface : bool) = Interop.mkAttr "useInterface" useInterface
    static member inline pdbId (pdbId : string) = Interop.mkAttr "pdbId" pdbId
    static member inline url (url : string) = Interop.mkAttr "url" url
    static member inline file (file : string) = Interop.mkAttr "file" file
    static member inline rawPdbText (rawPdbText : string) = Interop.mkAttr "rawPdbText" rawPdbText
    static member inline dimensions (dimensions : int * int) = Interop.mkAttr "dimensions" dimensions
    static member inline showControls (showControls : bool) = Interop.mkAttr "showControls" showControls
    static member inline showAxis (showAxis : bool) = Interop.mkAttr "showAxis" showAxis
    static member inline className (className : string) = Interop.mkAttr "className" className

[<Erase>]
type Molstar =
    static member inline molstar (properties : IReactProperty list) =
        Interop.reactApi.createElement(import "default" "./js/molstar.jsx", createObj !!properties)

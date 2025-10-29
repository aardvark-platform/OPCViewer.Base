namespace ViewPlanner
open Aardvark.Base
open Aardvark.Rendering
open FShade

module Shading =

     type Vertex =
        {
            [<Position>]                pos     : V4f
            [<TexCoord>]                tc      : V2f
            [<Color>]                   color   : V4f
            [<SourceVertexIndex>]       sv      : int
            [<Semantic("Tex0")>]        tc0     : V4f
        }

     type UniformScope with
        member x.FootprintMVP : M44f = uniform?FootprintMVP

     let vert (v: Vertex) =
        vertex {
            let mvp = uniform.FootprintMVP

            return 
                { v
                    with 
                        tc0 = mvp * v.pos 
                        pos = uniform.ModelViewProjTrafo * v.pos
                }
            }

     let frag (v: Vertex) =
        fragment {
            let clip = v.tc0 
            let low = -clip.W
            let upp = clip.W
            let col = 
                if (clip.X > low && clip.X < upp && clip.Y > low && clip.Y < upp && clip.Z > low && clip.Z < upp) then
                    V4f(0.0f, 0.0f, 1.0f, 1.0f)
                else
                    v.color

            return col
        }

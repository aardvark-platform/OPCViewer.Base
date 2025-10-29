namespace OpcViewer.Base

module Shader =
    open Aardvark.Rendering
    open Aardvark.Rendering.Effects
    open Aardvark.Base
    open FShade

    type SuperVertex = 
        {
            [<Position>] pos :  V4f
            [<SourceVertexIndex>] i : int
        }

    type AttrVertex =
        {
            [<Position>]                pos     : V4f            
            [<WorldPosition>]           wp      : V4f
            [<TexCoord>]                tc      : V2f
            [<Color>]                   c       : V4f
            [<Normal>]                  n       : V3f
            [<Semantic("Scalar")>]      scalar  : float32
            [<Semantic("LightDir")>]    ldir    : V3f
        }

    type PointVertex =
        {
            [<Position>] pos : V4f
            [<PointSize>] p : float32
            [<Color>] c : V4f
            [<TexCoord; Interpolation(InterpolationMode.Sample)>] tc : V2f
            [<SourceVertexIndex>] i : int
        }

    type VertexDepth = 
        {   
            [<Color>] c : V4f
            [<Depth>] d : float32
        }

    let lines (t : Triangle<SuperVertex>) =
        line {
            yield t.P0
            yield t.P1
            restartStrip()
            
            yield t.P1
            yield t.P2
            restartStrip()

            yield t.P2
            yield t.P0
            restartStrip()
        }

    module DebugColor = 
    
        type UniformScope with
            member x.UseDebugColor : bool = x?UseDebugColor
            member x.Color : C4b = x?Color
    
        let internal debugCol (_ : Vertex) =
            let c : C4b = uniform.Color
            let c1 = c.ToC4f().ToV4f().XYZ
            fragment {
                let useDebugColor : bool = uniform.UseDebugColor 
                if useDebugColor then
                    return V4f.IOOI
                else
                    return V4f(c1, 0.3f) 
            }

        let Effect = 
            toEffect debugCol

    module VertexCameraShift = 

        type UniformScope with
            member x.DepthOffset : float32 = x?DepthOffset

        let internal toCameraShift (p : Vertex) =
            vertex {
                let wp = p.wp
                let viewVec = (wp.XYZ - uniform.CameraLocation).Normalized
                let viewVec = V4f(viewVec.X, viewVec.Y, viewVec.Z, 0.0f)
                let wpShift = wp + viewVec * uniform.DepthOffset
                let posShift = uniform.ViewProjTrafo * wpShift

                return { p with pos = posShift; wp = wpShift }
            }

        let Effect = 
            toEffect toCameraShift

    module PointSprite = 
        let internal pointSprite (p : Point<Vertex>) =
            triangle {
                let s = uniform.PointSize / V2f uniform.ViewportSize
                let pos = p.Value.pos
                let pxyz = pos.XYZ / pos.W

                let p00 = V3f(pxyz + V3f( -s.X*0.33f, -s.Y, 0.0f ))
                let p01 = V3f(pxyz + V3f(  s.X*0.33f, -s.Y, 0.0f ))
                let p10 = V3f(pxyz + V3f( -s.X,      -s.Y*0.33f, 0.0f ))
                let p11 = V3f(pxyz + V3f(  s.X,      -s.Y*0.33f, 0.0f ))
                let p20 = V3f(pxyz + V3f( -s.X,       s.Y*0.33f, 0.0f ))
                let p21 = V3f(pxyz + V3f(  s.X,       s.Y*0.33f, 0.0f ))
                let p30 = V3f(pxyz + V3f( -s.X*0.33f,  s.Y, 0.0f ))
                let p31 = V3f(pxyz + V3f(  s.X*0.33f,  s.Y, 0.0f ))

                yield { p.Value with pos = V4f(p00 * pos.W, pos.W); tc = V2f (0.33f, 0.00f); }
                yield { p.Value with pos = V4f(p01 * pos.W, pos.W); tc = V2f (0.66f, 0.00f); }
                yield { p.Value with pos = V4f(p10 * pos.W, pos.W); tc = V2f (0.00f, 0.33f); }
                yield { p.Value with pos = V4f(p11 * pos.W, pos.W); tc = V2f (1.00f, 0.33f); }
                yield { p.Value with pos = V4f(p20 * pos.W, pos.W); tc = V2f (0.00f, 0.66f); }
                yield { p.Value with pos = V4f(p21 * pos.W, pos.W); tc = V2f (1.00f, 0.66f); }
                yield { p.Value with pos = V4f(p30 * pos.W, pos.W); tc = V2f (0.33f, 1.00f); }
                yield { p.Value with pos = V4f(p31 * pos.W, pos.W); tc = V2f (0.66f, 1.00f); }
            }

        let Effect = 
            toEffect pointSprite

    module PointSpriteQuad =       
        let internal pointSpriteQuad (p : Point<Vertex>) =
            triangle {
                let s = (uniform.PointSize / V2f uniform.ViewportSize)
                let pos = p.Value.pos
                let pxyz = pos.XYZ / pos.W

                let p00 = V3f(pxyz + V3f( -s.X, -s.Y, 0.0f ))
                let p01 = V3f(pxyz + V3f(  s.X, -s.Y, 0.0f ))
                let p10 = V3f(pxyz + V3f(  s.X,  s.Y, 0.0f ))
                let p11 = V3f(pxyz + V3f( -s.X,  s.Y, 0.0f ))

                yield { p.Value with pos = V4f(p00 * pos.W, pos.W); tc = V2f (0.00f, 0.00f); }
                yield { p.Value with pos = V4f(p01 * pos.W, pos.W); tc = V2f (1.00f, 0.00f); }
                yield { p.Value with pos = V4f(p11 * pos.W, pos.W); tc = V2f (0.00f, 1.00f); }          
                yield { p.Value with pos = V4f(p10 * pos.W, pos.W); tc = V2f (1.00f, 1.00f); }
            }
    
        let Effect = 
            toEffect pointSpriteQuad

    module AttributeShader = 

        type UniformScope with
            member x.Invert : bool = x?inverted
            member x.FalseColors : bool = x?falseColors
            member x.UseColors : bool = x?useColors
            member x.UpperBound : float32 = x?upperBound
            member x.LowerBound : float32 = x?lowerBound
            member x.Interval : float32 = x?interval
            member x.EndC : float32 = x?endC     //upperHueBound
            member x.StartC : float32 = x?startC //lowerHueBound

        [<ReflectedDefinition>]
        let hsv2rgb (h : float32) (s : float32) (v : float32) =
            let h = Fun.Frac(h)
            let chr = v * s
            let x = chr * (1.0f - Fun.Abs(Fun.Frac(h * 3.0f) * 2.0f - 1.0f))
            let m = v - chr
            let t = int (h * 6.0f)
            match t with
            | 0 -> V3f(chr + m, x + m, m)
            | 1 -> V3f(x + m, chr + m, m)
            | 2 -> V3f(m, chr + m, x + m)
            | 3 -> V3f(m, x + m, chr + m)
            | 4 -> V3f(x + m, m, chr + m)
            | 5 -> V3f(chr + m, m, x + m)
            | _ -> V3f(chr + m, x + m, m)

        [<ReflectedDefinition>]
        let mapFalseColors value : float32 =         
            let low         = if (uniform.Invert = false) then uniform.LowerBound else uniform.UpperBound
            let up          = if (uniform.Invert = false) then uniform.UpperBound else uniform.LowerBound
            let interval    = if (uniform.Invert = false) then uniform.Interval   else -1.0f * uniform.Interval        

            let rangeValue = up - low + interval
            let normInterv = (interval / rangeValue)

            //map range to 0..1 according to lower/upperbound
            let k = (value - low + interval) / rangeValue

            //discretize lookup
            let bucket = floor (k / normInterv)
            let k = (float32 bucket) * normInterv |> clamp 0.0f 1.0f

            let uH = uniform.EndC * 255.0f
            let lH = uniform.StartC * 255.0f
            //map values to hue range
            // let fcHueUpperBound = if (uH < lH) then uH + 1.0f else uH
            let rangeHue = uH - lH // fcHueUpperBound - lH
            (k * rangeHue) + lH
            
        let falseColorLegend (v : AttrVertex) =
            fragment {    

            if uniform.FalseColors then
                if uniform.UseColors then
                    let hue = mapFalseColors v.scalar 
                    let c = hsv2rgb ((clamp 0.0f 255.0f hue)/ 255.0f ) 1.0f 1.0f 
                    return v.c * V4f(c.X, c.Y, c.Z, 1.0f)
                else
                    let k = (v.scalar - uniform.LowerBound) / (uniform.UpperBound-uniform.LowerBound) 
                    let value = clamp 0.0f 1.0f k
                    return V4f(value, value, value, 1.0f) 
            else
                  return v.c
            }
  
        let falseColorLegendGray (v : AttrVertex) =
            fragment {   
                if uniform.FalseColors then
                    let k = (v.scalar - uniform.LowerBound) / (uniform.UpperBound-uniform.LowerBound) 
                    let value = clamp 0.0f 1.0f k
                    return V4f(value, value, value, 1.0f) 
                else
                    return v.c
            }

    module StableLight =
        let stableLight (v : AttrVertex) =
            fragment {
                let n = v.n |> Vec.normalize
                let c = v.ldir |> Vec.normalize
             
                let diffuse = Vec.dot c n |> abs            
 
                return V4f(v.c.XYZ * diffuse, v.c.W)
            }

        let Effect =
            toEffect stableLight

    module StableTrafo =

        [<ReflectedDefinition>]
        let transformNormal (n : V3f) =
            uniform.ModelViewTrafoInv.Transposed * V4f(n, 0.0f)
            |> Vec.xyz
            |> Vec.normalize

        let stableTrafo (v : AttrVertex) =
            vertex {
                let vp = uniform.ModelViewTrafo * v.pos
                let wp = uniform.ModelTrafo * v.pos
                return  
                    { v with
                        pos  = uniform.ProjTrafo * vp
                        wp   = wp
                        n    = transformNormal v.n
                        ldir = V3f.Zero - vp.XYZ |> Vec.normalize
                    } 
            } 


        let Effect = 
            toEffect stableTrafo

    module ThickLineNew = 
        type ThickLineVertex = 
            {
                [<Position>]                pos     : V4f
                [<Color>]                   c       : V4f
                [<Semantic("LineCoord")>]   lc      : V2f
                [<Semantic("Width")>]       w       : float32
                [<SourceVertexIndex>]       i : int
            }

        [<ReflectedDefinition>]
        let clipLine (plane : V4f) (p0 : ref<V4f>) (p1 : ref<V4f>) =
            let h0 = Vec.dot plane p0.Value
            let h1 = Vec.dot plane p1.Value
    
            // h = h0 + (h1 - h0)*t
            // 0 = h0 + (h1 - h0)*t
            // (h0 - h1)*t = h0
            // t = h0 / (h0 - h1)
            if h0 > 0.0f && h1 > 0.0f then
                false
            elif h0 < 0.0f && h1 > 0.0f then
                let t = h0 / (h0 - h1)
                p1.Value <- p0.Value + t * (p1.Value - p0.Value)
                true
            elif h1 < 0.0f && h0 > 0.0f then
                let t = h0 / (h0 - h1)
                p0.Value <- p0.Value + t * (p1.Value - p0.Value)
                true
            else
                true
    
        [<ReflectedDefinition>]
        let clipLinePure (plane : V4f) (p0 : V4f) (p1 : V4f) =
            let h0 = Vec.dot plane p0
            let h1 = Vec.dot plane p1
    
            // h = h0 + (h1 - h0)*t
            // 0 = h0 + (h1 - h0)*t
            // (h0 - h1)*t = h0
            // t = h0 / (h0 - h1)
            if h0 > 0.0f && h1 > 0.0f then
                (false, p0, p1)
            elif h0 < 0.0f && h1 > 0.0f then
                let t = h0 / (h0 - h1)
                let p11 = p0 + t * (p1 - p0)
                (true, p0, p11)
            elif h1 < 0.0f && h0 > 0.0f then
                let t = h0 / (h0 - h1)
                let p01 = p0 + t * (p1 - p0)
                
                (true, p01, p1)
            else
                (true, p0, p1)
    
        let thickLine (line : Line<ThickLineVertex>) =
            triangle {
                let t = uniform.LineWidth
                let sizeF = V3f(float32 uniform.ViewportSize.X, float32 uniform.ViewportSize.Y, 1.0f)
    
                let mutable pp0 = line.P0.pos
                let mutable pp1 = line.P1.pos        
                                
                let add = 2.0f * V2f(t,t) / sizeF.XY
                                
                let a0 = clipLine (V4f( 1.0f,  0.0f,  0.0f, -(1.0f + add.X))) &&pp0 &&pp1
                let a1 = clipLine (V4f(-1.0f,  0.0f,  0.0f, -(1.0f + add.X))) &&pp0 &&pp1
                let a2 = clipLine (V4f( 0.0f,  1.0f,  0.0f, -(1.0f + add.Y))) &&pp0 &&pp1
                let a3 = clipLine (V4f( 0.0f, -1.0f,  0.0f, -(1.0f + add.Y))) &&pp0 &&pp1
                let a4 = clipLine (V4f( 0.0f,  0.0f,  1.0f, -1.0f)) &&pp0 &&pp1
                let a5 = clipLine (V4f( 0.0f,  0.0f, -1.0f, -1.0f)) &&pp0 &&pp1
    
                if a0 && a1 && a2 && a3 && a4 && a5 then
                    let p0 = pp0.XYZ / pp0.W
                    let p1 = pp1.XYZ / pp1.W
    
                    let fwp = (p1.XYZ - p0.XYZ) * sizeF
    
                    let fw = V3f(fwp.XY, 0.0f) |> Vec.normalize
                    let r = V3f(-fw.Y, fw.X, 0.0f) / sizeF
                    let d = fw / sizeF
                    let p00 = p0 - r * t - d * t
                    let p10 = p0 + r * t - d * t
                    let p11 = p1 + r * t + d * t
                    let p01 = p1 - r * t + d * t
    
                    let rel = t / (Vec.length fwp)
    
                    yield { line.P0 with i = 0; pos = V4f(p00 * pp0.W, pp0.W); lc = V2f(-1.0f, -rel); w = rel }      // restore W component for depthOffset
                    yield { line.P0 with i = 0; pos = V4f(p10 * pp1.W, pp1.W); lc = V2f( 1.0f, -rel); w = rel }      // restore W component for depthOffset
                    yield { line.P1 with i = 1; pos = V4f(p01 * pp0.W, pp0.W); lc = V2f(-1.0f, 1.0f + rel); w = rel } // restore W component for depthOffset
                    yield { line.P1 with i = 1; pos = V4f(p11 * pp1.W, pp1.W); lc = V2f( 1.0f, 1.0f + rel); w = rel } // restore W component for depthOffset
            }
    
        let Effect =
            toEffect thickLine

    module DepthOffset =
        
        type UniformScope with
            member x.DepthOffset : float32 = x?DepthOffset
        
        [<GLSLIntrinsic("(gl_DepthRange.far - gl_DepthRange.near)")>]
        let depthDiff()  : float32 = onlyInShaderCode ""
    
        [<GLSLIntrinsic("gl_DepthRange.near")>]
        let depthNear()  : float32 = onlyInShaderCode ""
    
        [<GLSLIntrinsic("gl_DepthRange.far")>]
        let depthFar()  : float32 = onlyInShaderCode ""

        let depthOffsetFS (v : Vertex) =
            fragment {
                let d = (v.pos.Z - uniform.DepthOffset)  / v.pos.W
                return { c = v.c;  d = ((depthDiff() * d) + depthNear() + depthFar()) / 2.0f  }
            }

        let Effect =
            toEffect depthOffsetFS

    module TriangleFilter = 
    
        type UniformScope with
            member x.TriangleSize : float32 = x?TriangleSize
    
        let triangleFilter'' (input : Triangle<Vertex>) =
            triangle {
                let p0 = input.P0.wp.XYZ
                let p1 = input.P1.wp.XYZ
                let p2 = input.P2.wp.XYZ
                let cross = Vec.cross (p1 - p0) (p2 - p0)            
                let area = cross.LengthSquared
                let maxArea = uniform.TriangleSize
                let check = area < maxArea
                if check then
                    yield input.P0 
                    yield input.P1
                    yield input.P2
            }

        let triangleFilter' (input : Triangle<Vertex>) =
            triangle {
                let A = input.P0.wp.XYZ
                let B = input.P1.wp.XYZ
                let C = input.P2.wp.XYZ

                let maxRel = uniform.TriangleSize

                let a = (B - A).Length < maxRel
                let b = (C - B).Length < maxRel
                let c = (A - C).Length < maxRel                        

                let check = a && b && c
                if check then
                    yield input.P0 
                    yield input.P1
                    yield input.P2
            }

        let triangleFilter (input : Triangle<Vertex>) =
            triangle {
                let p0 = input.P0.wp.XYZ
                let p1 = input.P1.wp.XYZ
                let p2 = input.P2.wp.XYZ

                let maxRel = uniform.TriangleSize

                let a = (p1 - p0).Normalized
                let b = (p2 - p1).Normalized
                let c = (p0 - p2).Normalized          

                let alpha = (a.Dot b).Abs() < maxRel
                let beta  = (b.Dot c).Abs() < maxRel
                let gamma = (c.Dot a).Abs() < maxRel

                let check = (alpha && beta && gamma) || maxRel >= 0.999999999999999f
                if check then
                    yield input.P0 
                    yield input.P1
                    yield input.P2
            }
    
        let Effect0 = toEffect triangleFilter
        let Effect1 = toEffect triangleFilter'
        let Effect2 = toEffect triangleFilter''

    module ScreenSpaceScale =
    
        type UniformScope with
            member x.Size : float32 = x?Size
            member x.WorldPos : V3f = x?WorldPos
    
        let screenSpaceScale (v : Vertex) =
            vertex {        
                let loc     = uniform.CameraLocation       
                let hvp    = float32 uniform.ViewportSize.X

                let dist = (uniform.WorldPos - loc).Length      
                let scale = dist * uniform.Size / hvp 

                return 
                    { v with
                        pos = V4f(v.pos.X * scale, v.pos.Y * scale, v.pos.Z * scale, v.pos.W)
                    }
            }

        let Effect =
            toEffect screenSpaceScale

    module SelectionColor =
    
        type UniformScope with
            member x.Selected : bool = x?selected
    
        let selectionColor (v : Vertex) =
            fragment {
                if uniform.Selected then
                    //let c : V4f = uniform?selectionColor
                    //return c * v.c
                    let gamma = 1.3f
                    return V4f(v.c.X ** (1.0f / gamma), v.c.Y ** (1.0f / gamma),v.c.Z ** (1.0f / gamma), 1.0f)
                else return v.c
            }

        let Effect = 
            toEffect selectionColor

    module LoDColor = 
    
        type UniformScope with
            member x.LodVisEnabled : bool = x?LodVisEnabled
            member x.LoDColor : V4f = x?LoDColor

        let LoDColor  (v : Vertex) =
            fragment {
                if uniform.LodVisEnabled then
                    let c : V4f = uniform.LoDColor
                    let gamma = 1.0f
                    let grayscale = 0.2126f * v.c.X ** gamma + 0.7152f * v.c.Y ** gamma  + 0.0722f * v.c.Z ** gamma 
                    return grayscale * c
                else return v.c
            }

        let Effect =
            toEffect LoDColor

    module FalseColorGeoSpatial = 
    
        type UniformScope with
            member x.ColorMapTexture : ShaderTextureHandle = x?ColorMapTexture
            member x.FalseColors : bool = x?falseColors
            member x.MinMax : V2f = x?MinMax


        let private colormap =
            sampler2d {
                texture uniform.ColorMapTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
        }

        let falseColor (v : AttrVertex) =
            fragment {           
                if uniform.FalseColors then
                    let range = uniform.MinMax
                    let norm = V2f((v.scalar - range.X)/ (range.Y - range.X), 0.5f)

                    let c = colormap.Sample(norm).XYZ

                    return v.c * V4f(c.X, c.Y, c.Z, 1.0f)
                else
                    return v.c
            }

        let Effect =
            toEffect falseColor

//Pro3d shaders...(simplified)
    module PointSize = 

        type UniformScope with
            member x.PointSize : float32 = uniform?PointSize

        let pointTrafo (v : PointVertex) =
            vertex {
                let ps : float32 = uniform.PointSize
                let vp = uniform.ModelViewTrafo * v.pos
                return { 
                    v with 
                        pos = uniform.ProjTrafo * vp
                        p = ps
                }
            }

        let pointSpriteFragment (v : PointVertex) =
            fragment {
                let tc = v.tc

                let c = 2.0f * tc - V2f.II
                if c.Length > 1.0f then
                    discard()

                return v
            }

        let lines (t : Triangle<PointVertex>) =
            line {
                yield t.P0
                yield t.P1
                restartStrip()
            
                yield t.P1
                yield t.P2
                restartStrip()

                yield t.P2
                yield t.P0
                restartStrip()
            }

        let EffectPointTrafo =
            toEffect pointTrafo
    
        let EffectPointSpriteFragment = 
            toEffect pointSpriteFragment

        let EffectLines =
            toEffect lines

    module OPCFilter =
        type UniformScope with
            member x.DiffuseColorTexture : ShaderTextureHandle = uniform?DiffuseColorTexture

        let private diffuseSampler =
            sampler2d {
                texture uniform.DiffuseColorTexture          
                filter Filter.Anisotropic
                maxAnisotropy 16
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let improvedDiffuseTexture (v : Effects.Vertex) =
            fragment {
                let texColor = diffuseSampler.Sample(v.tc,-1.0f)
                return texColor
            }

        let EffectOPCFilter =
            toEffect improvedDiffuseTexture

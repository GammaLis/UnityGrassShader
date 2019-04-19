Shader "Roystan/Grass"
{
    Properties
    {
		[Header(Shading)]
        _TopColor("Top Color", Color) = (1,1,1,1)
		_BottomColor("Bottom Color", Color) = (1,1,1,1)
		_TranslucentGain("Translucent Gain", Range(0,1)) = 0.5
    }

	CGINCLUDE
	#include "UnityCG.cginc"
	#include "Autolight.cginc"

	struct GeometryOutput
	{
		float4 pos : SV_POSITION;
	};

	/**
		2 params
	point float4 IN[1] - states that we will take a single point in as our input
	TriangleStream - sets up our shader to output a stream of triangles
	
	[maxvertexcount(3)]- tells the GPU that we will emit(but are not required to)
	at most 3 vertices
	*/
	[maxvertexcount(3)]
	void geo (point float4 IN[1] : SV_POSITION, inout TriangleStream<GeometryOutput> triStream)
	{
		// Just emit a triangle
		GeometryOutput o;
		o.pos = float4(0.5, 0, 0, 1);
		triStream.Append(o);

		o.pos = float4(-0.5, 0, 0, 1);
		triStream.Append(o);

		o.pos = float4(0, 1, 0, 1);
		triStream.Append(o);
	}

	// Simple noise function, sourced from http://answers.unity.com/answers/624136/view.html
	// Extended discussion on this function can be found at the following link:
	// https://forum.unity.com/threads/am-i-over-complicating-this-random-function.454887/#post-2949326
	// Returns a number in the 0...1 range.
	float rand(float3 co)
	{
		return frac(sin(dot(co.xyz, float3(12.9898, 78.233, 53.539))) * 43758.5453);
	}

	// Construct a rotation matrix that rotates around the provided axis, sourced from:
	// https://gist.github.com/keijiro/ee439d5e7388f3aafc5296005c8c3f33
	float3x3 AngleAxis3x3(float angle, float3 axis)
	{
		float c, s;
		sincos(angle, s, c);

		float t = 1 - c;
		float x = axis.x;
		float y = axis.y;
		float z = axis.z;

		return float3x3(
			t * x * x + c, t * x * y - s * z, t * x * z + s * y,
			t * x * y + s * z, t * y * y + c, t * y * z - s * x,
			t * x * z - s * y, t * y * z + s * x, t * z * z + c
			);
	}

	float4 vert(float4 vertex : POSITION) : SV_POSITION
	{
		return UnityObjectToClipPos(vertex);
	}
	ENDCG

    SubShader
    {
		Cull Off

        Pass
        {
			Tags
			{
				"RenderType" = "Opaque"
				"LightMode" = "ForwardBase"
			}

            CGPROGRAM
            #pragma vertex vert
			#pragma geometry geo 	// 几何着色器
            #pragma fragment frag
			#pragma target 4.6
            
			#include "Lighting.cginc"

			float4 _TopColor;
			float4 _BottomColor;
			float _TranslucentGain;

			float4 frag (float4 vertex : SV_POSITION, fixed facing : VFACE) : SV_Target
            {	
				return float4(1, 1, 1, 1);
            }
            ENDCG
        }
    }
}

/**
	Geometry shaders
	Geometry shaders are an optional part of the rendering pipeline. They are executed after
the veretex shader (or the tessellation shader - if tessellation is being used), and 
before the vertices are processed for the fragment shader.

	Geometry shaders take in a single primitives as input, and can generate zero, one,
or many primitives.
*/

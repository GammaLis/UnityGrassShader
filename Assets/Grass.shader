// https://roystan.net/articles/grass-shader.html
Shader "Roystan/MyGrass"
{
    Properties
    {
		[Header(Shading)]
		// widht & height
		_BladeWidth ("Blade Width", Float) = 0.05
		_BladeWidthRandom ("Blade Width Random", Float) = 0.02
		_BladeHeight ("Blade Height", Float) = 0.5
		_BladeHeightRandom ("Blade Height Random", Float) = 0.3
		// color gradient
        _TopColor ("Top Color", Color) = (1,1,1,1)
		_BottomColor ("Bottom Color", Color) = (1,1,1,1)
		_TranslucentGain ("Translucent Gain", Range(0,1)) = 0.5
		// blend rotation
		_BendRotationRandom ("Bend Rotation Random", Range(0, 1)) = 0.2
		// tessellation
		_TessellationUniform ("Tessellation Uniform", Range(1, 64)) = 1
		// wind
		_WindDistortionMap ("Wind Distortion Map", 2D) = "white" {}
		_WindFrequency ("Wind Frequency", Vector) = (0.05, 0.05, 0, 0)
		_WindStrength ("Wind Strength", Float) = 1
		// blade curvature
		_BladeForward ("Blade Forward Amount", Float) = 0.38
		_BladeCurve ("Blade Curvature Amount", Range(1, 4)) = 2
		// lower valuds of _BladeForward and _BladeCurve will result in a more
		// organized, well tended field of grass, while larger values will have the 
		// opposite effect
    }

	CGINCLUDE
	#include "UnityCG.cginc"
	#include "Autolight.cginc"
	
	#include "Shaders/CustomTessellation.cginc"

	// CustomTessellation.cginc already defined
	// struct vertexInput
	// {
	// 	float4 vertex : POSITION;
	// 	float3 normal : NORMAL;
	// 	float4 tangent : TANGENT;
	// };

	// struct vertexOutput
	// {
	// 	float4 pos : SV_POSITION;
	// 	float3 normal : NORMAL;
	// 	float4 tangent : TANGENT;
	// };

	struct geometryOutput
	{
		float4 pos : SV_POSITION;
		float3 normal : NORMAL;
		float2 uv : TEXCOORD0;
		unityShadowCoord4 _ShadowCoord : TEXCOORD1;
		// SHADOW_COORDS(1);
	};

	// width & height
	float _BladeWidth;
	float _BladeWidthRandom;
	float _BladeHeight;
	float _BladeHeightRandom;

	float _BendRotationRandom;

	// wind
	sampler2D _WindDistortionMap;
	float4 _WindDistortionMap_ST;
	float2 _WindFrequency;
	float _WindStrength;

	// blade curvature
	float _BladeForward;
	float _BladeCurve;

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

	geometryOutput VertexToGeometry (float3 pos, float2 uv, float3 normal)
	{
		geometryOutput o;
		o.pos = UnityObjectToClipPos(pos);
		o.normal = normalize(mul(normal, (float3x3)unity_WorldToObject));
			// o.normal = UnityObjectToWorldNormal(normal);
		o.uv = uv;
		o._ShadowCoord = ComputeScreenPos(o.pos);
		#if UNITY_PASS_SHADOWCASTER
			// Applying the bias prevents artifacts from appearing on the surfaces
			o.pos = UnityApplyLinearShadowBias(o.pos);
		#endif
		return o;
	}

	/**
		2 params
	point float4 IN[1] - states that we will take a single point in as our input
	TriangleStream - sets up our shader to output a stream of triangles
	
	[maxvertexcount(3)]- tells the GPU that we will emit(but are not required to)
	at most 3 vertices
	*/
	[maxvertexcount(3)]
	void geo_Ver1 (point vertexOutput IN[1] : SV_POSITION, inout TriangleStream<geometryOutput> triStream)
	{
		// Just emit a triangle
		// Our triangle is now correctly rendered in the world. However, it appears that only 
		// one is being created.
		// In actually, a triangle is being drawn for each vertex in our mesh.
		// ------------------
		// ver. 1
		// geometryOutput o;
		// o.pos = UnityObjectToClipPos(float4(0.5, 0, 0, 1));
		// triStream.Append(o);

		// o.pos = UnityObjectToClipPos(float4(-0.5, 0, 0, 1));
		// triStream.Append(o);

		// o.pos = UnityObjectToClipPos(float4(0, 1, 0, 1));
		// triStream.Append(o);

		// ver. 2
		// geometryOutput o;
		// float3 pos = IN[0].vertex;

		// o.pos = UnityObjectToClipPos(pos + float3(0.5, 0, 0));
		// triStream.Append(o);

		// o.pos = UnityObjectToClipPos(pos + float3(-0.5, 0, 0));
		// triStream.Append(o);

		// o.pos = UnityObjectToClipPos(pos + float3(0, 1, 0));
		// triStream.Append(o);
		// right now, the triangles are all being emitted in the same direction, rather than
		// outwards from the surface of the sphere. To resolve this, we will construct our grass 
		// blades in tangent space.

		// ver. 3
		geometryOutput o;
		float3 pos = IN[0].vertex;
		float3 normal = IN[0].normal;
		float4 tangent = IN[0].tangent;
		float3 binormal = cross(normal, tangent.xyz) * tangent.w;
		/**
			Why is the result of the cross product multiplied by the tangent's w coordinate?
			When a mehs is exported from a 3D modeling package, it usually has the binomrals already
		stored in the mesh data. Instead of importing these binormals, Unity simply grabs each binormals
		direction and assigns it to the tangent's w coordinate. This has the benefit of saving
		on memory, while still ensuring the correct binormal can be reconstructed later.
		*/
		float3x3 tangentToLocal = float3x3(
			tangent.x, binormal.x, normal.x,
			tangent.y, binormal.y, normal.y,
			tangent.z, binormal.z, normal.z
		);

		// <<Wind>>
		// we apply the _WindDistortionMap scale and offset to our position, and then
		// further offset it by _Time.y, scaled by _WindFrequency
		float2 uv = pos.xz * _WindDistortionMap_ST.xy + _WindDistortionMap_ST.zw + 
					_WindFrequency * _Time.y;
		float2 windSample = tex2Dlod(_WindDistortionMap, float4(uv, 0, 0)).xy * 2 - 1 * _WindStrength;
		// we can construct a normalized vector representing the direction of the wind
		float3 wind = normalize(float3(windSample.x, windSample.y, 0));
		// wind = normalize(float3(windSample.y, -windSample.x, 0)); 	// my wind direction

		// we can now construct a matrix to rotate about this vector, and multiply it into
		// our transformationMat
		float3x3 windRotation = AngleAxis3x3(UNITY_PI * windSample, wind);

		// <<Facing Rotation>>
		// we use the input position pos as the random seed for our rotation, this way, every blade will
		// get a different rotation, but it will be consistent between frames.
		float3x3 facingRotationMat = AngleAxis3x3(rand(pos) * UNITY_TWO_PI, float3(0, 0, 1));
		// <<Bend Rotation>>
		// multiply UNITY_PI by 0.5, this gives us a random range of 0-90 degrees
		float3x3 bendRotationMat = AngleAxis3x3(rand(pos.zzx) * _BendRotationRandom * UNITY_PI * 0.5, float3(-1, 0, 0));

		// 运算关系类似 = (F * B * inv(F)) * F = F * B
		float3x3 transformationMat = mul(tangentToLocal, mul(facingRotationMat, bendRotationMat));
		// float3x3 transformationMat = mul(tangentToLocal, mul(bendRotationMat, facingRotationMat)); 	// 这样不对

		transformationMat = mul(mul(mul(tangentToLocal, windRotation), 
			facingRotationMat), bendRotationMat);

		// only include Wind Rotation
		// transformationMat = mul(tangentToLocal, windRotation);

		float3x3 transformationFacing = mul(tangentToLocal, facingRotationMat);

		transformationMat = transformationFacing;

		float width = (rand(pos.xyz) * 2 - 1) * _BladeWidthRandom + _BladeWidth;
		float height = (rand(pos.zyx) * 2 - 1) * _BladeHeightRandom + _BladeHeight;

		// temp local normal (can be deleted)
		float3 tangentNormal = float3(0, -1, 1);
		float3 localNormal = mul(transformationMat, tangentNormal);
		// end

		triStream.Append(VertexToGeometry(pos + mul(transformationMat, float3(width, 0, 0)), float2(0, 0), localNormal));
		triStream.Append(VertexToGeometry(pos + mul(transformationMat, float3(-width, 0, 0)), float2(1, 0), localNormal));
		triStream.Append(VertexToGeometry(pos + mul(transformationMat, float3(0, 0, height)), float2(0.5, 1), localNormal));

		// up until now, we've only outputted 3 vertices, making for a single triangle.
		// triangle strip
		// The first 3 vertices are connected to form a triangle-as before-with each additonal
		// vertex forming a triangle with the previous 2.
	}

	geometryOutput GenerateGrassVertex (float3 vertexPos, float width, float height, 
		float forward, float2 uv, float3x3 transformationMat)
	{
		// pos
		float3 tangentPoint = float3(width, forward, height);
		// normal
		float3 tangentNormal = float3(0, -1, forward);

		float3 localPos = vertexPos + mul(transformationMat, tangentPoint);
		float3 localNormal = mul(transformationMat, tangentNormal);
		return VertexToGeometry(localPos, uv, localNormal);
	}

	#define BLADE_SEGMENTS 3
	[maxvertexcount(BLADE_SEGMENTS * 2 + 1)]
	void geo (point vertexOutput IN[1] : SV_POSITION, inout TriangleStream<geometryOutput> triStream)
	{
		geometryOutput o;
		float3 pos = IN[0].vertex;
		float3 normal = IN[0].normal;
		float4 tangent = IN[0].tangent;
		float3 binormal = cross(normal, tangent.xyz) * tangent.w;

		float3x3 tangentToLocal = float3x3(
			tangent.x, binormal.x, normal.x,
			tangent.y, binormal.y, normal.y,
			tangent.z, binormal.z, normal.z
		);

		// <<Wind>>
		// we apply the _WindDistortionMap scale and offset to our position, and then
		// further offset it by _Time.y, scaled by _WindFrequency
		float2 uv = pos.xz * _WindDistortionMap_ST.xy + _WindDistortionMap_ST.zw + 
					_WindFrequency * _Time.y;
		float2 windSample = tex2Dlod(_WindDistortionMap, float4(uv, 0, 0)).xy * 2 - 1 * _WindStrength;
		// we can construct a normalized vector representing the direction of the wind
		float3 wind = normalize(float3(windSample.x, windSample.y, 0));

		// we can now construct a matrix to rotate about this vector, and multiply it into
		// our transformationMat
		float3x3 windRotation = AngleAxis3x3(UNITY_PI * windSample, wind);

		// <<Facing Rotation>>
		// we use the input position pos as the random seed for our rotation, this way, every blade will
		// get a different rotation, but it will be consistent between frames.
		float3x3 facingRotationMat = AngleAxis3x3(rand(pos) * UNITY_TWO_PI, float3(0, 0, 1));
		// <<Bend Rotation>>
		// multiply UNITY_PI by 0.5, this gives us a random range of 0-90 degrees
		float3x3 bendRotationMat = AngleAxis3x3(rand(pos.zzx) * _BendRotationRandom * UNITY_PI * 0.5, float3(-1, 0, 0));

		// 运算关系类似 = (F * B * inv(F)) * F = F * B
		float3x3 transformationMat = mul(tangentToLocal, mul(facingRotationMat, bendRotationMat));

		transformationMat = mul(mul(mul(tangentToLocal, windRotation), 
			facingRotationMat), bendRotationMat);

		float3x3 transformationFacing = mul(tangentToLocal, facingRotationMat);

		float width = (rand(pos.xyz) * 2 - 1) * _BladeWidthRandom + _BladeWidth;
		float height = (rand(pos.zyx) * 2 - 1) * _BladeHeightRandom + _BladeHeight;

		float forward = rand(pos.yyz) * _BladeForward;

		for (int k = 0; k < BLADE_SEGMENTS; ++k)
		{
			float t = k / (float)BLADE_SEGMENTS;
			float segmentWidth = width * (1 - t);
			float segmentHeight = height * t;
			float segemntForward = pow(t, _BladeCurve) * forward;
			// as we move up the blade, the height increases, and the width decreases

			// declaring transformMat- here we select between our 2 transformation matrice,
			// taking transformationFacing for the vertices at the base, and transformationMat
			// for all others
			float3x3 trnasformMat = k == 0 ? transformationFacing : transformationMat;
			triStream.Append(GenerateGrassVertex(pos, segmentWidth, segmentHeight, segemntForward, float2(0, t), trnasformMat));
			triStream.Append(GenerateGrassVertex(pos, -segmentWidth, segmentHeight, segemntForward, float2(1, t), trnasformMat));
		}
		triStream.Append(GenerateGrassVertex(pos, 0, height, forward, float2(0.5, 1), transformationMat));

		// triStream.Append(GenerateGrassVertex(pos, width, 0, float2(0, 0), transformationFacing));
		// triStream.Append(GenerateGrassVertex(pos, -width, 0, float2(1, 0), transformationFacing));
		// triStream.Append(GenerateGrassVertex(pos, 0, height, float2(0.5, 1), transformationFacing));
	}

	// ver. 1
	// float4 vert(float4 vertex : POSITION) : SV_POSITION
	// {
	// 	// return UnityObjectToClipPos(vertex);
	// 	return vertex;
	// }

	// CustomTessellation.cginc already defined
	// ver. 2
	// vertexOutput vert (vertexInput v)
	// {
	// 	vertexOutput o;
	// 	o.pos = v.vertex;
	// 	o.normal = v.normal;
	// 	o.tangent = v.tangent;
	// 	return o;
	// }
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
			
			#pragma hull hull
			#pragma domain domain

			#pragma geometry geo 	// 几何着色器
            #pragma fragment frag
			#pragma target 4.6

			#pragma multi_compile_fwdbase
            
			#include "Lighting.cginc"

			float4 _TopColor;
			float4 _BottomColor;
			float _TranslucentGain;

			// we can now sample our top and bottom colors in the fragment shader using
			// the UV, and interpolate between them using lerp.
			float4 frag (geometryOutput i, fixed facing : VFACE) : SV_Target
            {	
				// return float4(1, 1, 1, 1);

				// return lerp(_BottomColor, _TopColor, i.uv.y);

				// return SHADOW_ATTENUATION(i);

				// because our shader has Cull set to Off, both sides of the blade of grass
				// are rendered.
				// The fixed facing argument will return a positive number if we are viewing
				// the front of the surface, and a negative if we are viewing the back.
				float3 normal = facing > 0 ? i.normal : -i.normal;
				
				float shadow = SHADOW_ATTENUATION(i);
				float NdotL = saturate(saturate(dot(normal, _WorldSpaceLightPos0)) + _TranslucentGain) *
					shadow;
				
				float3 ambient = ShadeSH9(float4(normal, 1));
				float lightIntensity = NdotL * _LightColor0 + float4(ambient, 1);
				float4 col = lerp(_BottomColor, _TopColor * lightIntensity, i.uv.y);


				return col;
            }
            ENDCG
        }

		Pass
		{
			Tags
			{
				"LightMode" = "ShadowCaster"
				// this communicates to Unity that this pass should be used for
				// rendering the object to shadow maps.
			}

			CGPROGRAM
			#pragma vertex vert
			#pragma hull hull
			#pragma domain domain
			#pragma geometry geo
			#pragma fragment frag
			#pragma target 4.6
			#pragma multi_compile_shadowcaster
			// this ensures that the shader compiles all necessary variants required
			// for shadow casting

			fixed frag (geometryOutput i) : SV_TARGET
			{
				SHADOW_CASTER_FRAGMENT(i);
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

/**
	Random facing direction
	To create variation and add a more natural look, we'll next make every blade of grass
face a random direction. To do this, we'll need to construct a rotation matrix that rotates
a random amount  around the blade's up axis.
*/

/**
	Random forward bend
	If the blades of grass all stand up perfectly straight, they look very uniform. This
my be desireable for well tended grass, like on a putting green, but does not accurately represent
grass in the wild. We'll create a new matrix to rotate the grass along its X axis, and a 
property to control this rotation.
*/

/**
	The triangles are far too few.
	One solution would be to create a new, denser mesh, either through C# or using 3D
modeling software. While this would work, it would not allow for dynamic control of the 
grass density. Instead we will subdivide the input mesh using tessellation.

	Tessellation
	Tessellation is an optional stage in the rendering pipeline that occurs after the vertex
shader, and before the geometry shader. Its job is to subdivide a single input surface
into a many primitives. Tessellation is implemented with 2 programmable stages: the hull and 
domain shaders.
*/

/**
	Wind
	we will implement wind by sampling a distortion texture. This texture will be similar
to a normal map, except with only 2 channels(red and green). We will use these 2 channels as 
the X and Y directions of the wind.
	Before sampling the wind texture, we'll need to construct a UV coordinate. Rather than
using texture coordinates assigned to the mesh, we'll use the input point's position.
This way, if there are multiple grass meshes in the world, this will create the illusion 
they are all part of the same wind system.
	As well, we'll use the built_in shader variable _Time to scroll the wind texture along
our grass surface.
*/

/**
	Blade Curvature（弯曲）
	Right now, our individual blades of grass are defined by a single triangle. While this is 
not a problem at long distances, up close the blades look overly rigid and geometric, rather than
organic and alive.
	We'll correct this by instead constructing our blades with several triangles and bending them
along a curve.
*/

/**
	Lighting and Shadows

	1. Casting shadows
	in order to cast shadows in Unity, a second pass must be added to the shader. This pass will 
be used by shadow casting lights in the scene to render the grass's depth to their shadow map.

	2. Receiving Shadows
	After Unity renders a shadow map from the perspective of a shadow casting light, it will
run a pass "collect" the shadows into a screen space texture. To sample this texture,
we'll need to calculate the screen space positions of our vertices and pass them 
into the fragment shader.

	3. Lighting
	I = Dot (N, L)
*/

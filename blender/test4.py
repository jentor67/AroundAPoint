#!/usr/bin/python3

import bpy
import bmesh



# Get the active mesh
me = bpy.context.object.data


# Get a BMesh representation
bm = bmesh.new()   # create an empty BMesh
bm.from_mesh(me)   # fill it in from a Mesh


# Modify the BMesh, can do anything here...
i = 0
for v in bm.verts:
    print(i,v)
    i +=  1
    v.co.x += 1.0

print(v.co.x)

# Finish up, write the bmesh back to the mesh
bm.to_mesh(me)
bm.free()  # free and prevent further access

#!/usr/bin/python3

import bpy

# get absolute position of a vertex from an object with shapekeys (and other transforms)
dg = bpy.context.evaluated_depsgraph_get()
obj = bpy.context.object.evaluated_get(dg)

mesh = obj.to_mesh(preserve_all_data_layers=True, depsgraph=dg)
co = mesh.vertices[0].co
co_final = obj.matrix_world @ co
print(co_final)


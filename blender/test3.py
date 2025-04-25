#!/usr/bin/python3

import bpy
import bmesh

ground = bpy.context.object
layer = bpy.context.view_layer

def transform_ground_to_world(layer, ground):
    bm = bmesh.new()
    bm.from_object(ground, bpy.context.evaluated_depsgraph_get())
    tmp_ground_data=bpy.data.meshes.new(name='Mesh')
    bm.to_mesh(tmp_ground_data)
    tmp_ground = bpy.data.objects.new(name='tmpGround', object_data=tmp_ground_data)
    bm.transform(ground.matrix_world)
    bm.clear()
    bm.free()
    layer.active_layer_collection.collection.objects.link(tmp_ground)
    layer.update()
    return tmp_ground

transform_ground_to_world(layer, ground)


#!/usr/bin/python3

import bpy
import bmesh

mesh = bpy.data.meshes.new("mesh")
obj = bpy.data.objects.new("Function", mesh)

#scene = bpy.context.scene
#scene.objects.link(obj)
#scene.objects.active = obj
#obj.select = True

mesh = bpy.context.object.data
bm = bmesh.new()

goal=16 #right member of the equation
toll=5 #threshold

for x in range(-100,100):
    for y in range(-100,100):
        for z in range(-100,100):
            function=2*x**2+y**2-5*z**2+z-7*x #function here
            if((function<(goal+toll))and(function>(goal-toll))):
                bm.verts.new([x,y,z])

bm.to_mesh(mesh)
bm.free()

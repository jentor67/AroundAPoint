#!/usr/bin/python3
import bpy, csv

fp = "/home/jmajor/data.txt"

with open( fp ) as csvfile:
    rdr = csv.reader( csvfile )
    for i, row in enumerate( rdr ):
        x, y, z = row[0:3]
        print(x,y,z)
        # Generate UV sphere at x = lon and y = lat (and z = 0 )
        #bpy.ops.mesh.primitive_uv_sphere_add( location = ( float(lon), float(lat), 0 ) )

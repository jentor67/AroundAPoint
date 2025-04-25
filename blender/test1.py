#!/usr/bin/python3

#!/bin/true
# vim: se fo=tcroq tw=78 :
# Simple animated 3D plot example using Blender ( https://www.blender.org/ ).
# Given the function f(k, x, t)=exp(ikx-iωt), plots Re(f) against x and k,
# with colour given by Im(f) and t being the time.
#
# For pedagogical purposes, this just computes f at each frame (twice: once
# for the vertex positions and once for their colours). This is horribly
# inefficient; it would be much better to generate a 3D array for f(x, k, t)
# once and slice this array for each frame -- however, this is left as an
# exercise to the reader.
#
# To run, call
#   blender --python blenderplot-ani.py

import os.path

import numpy as np

import bpy

### Begin user settings
omega = 1
#font = '/usr/share/fonts/cm-unicode/cmunti.ttf' # Must be a unicode font!
font = '/usr/share/fonts/truetypye/samyak-fonts/Samyak-Gujarati.ttf' # Must be a unicode font!
### End user settings

### Begin generic Blender rendering code
# Global object counter.
obj_ind = 10000

plot_id = None

line_material = bpy.data.materials.new('line')
line_material.diffuse_color = (0, 0, 0)
line_material.diffuse_shader = 'LAMBERT'
line_material.specular_color = (0, 0, 0)
line_material.specular_shader = 'COOKTORR'
line_material.use_shadows = False
line_material.use_cast_shadows = False
line_material.use_raytrace = True
line_material.ambient = 0

text_material = bpy.data.materials.new('text')
text_material.diffuse_color = (.15, .05, .035)
text_material.diffuse_shader = 'OREN_NAYAR'
text_material.diffuse_intensity = .9
text_material.roughness = 2
text_material.specular_color = (.6, .2, .1)
text_material.specular_shader = 'PHONG'
text_material.specular_hardness = 80
text_material.specular_intensity = .85
text_material.use_shadows = True
text_material.use_cast_shadows = False
text_material.use_raytrace = True
text_material.raytrace_mirror.use = True
text_material.mirror_color = (.7, .3, .15)
text_material.raytrace_mirror.reflect_factor = .3
text_material.emit = 0
text_material.ambient = 0

plot_material = bpy.data.materials.new('plot')
plot_material.specular_color = (.5, .5, .5)
plot_material.specular_shader = 'COOKTORR'
plot_material.specular_intensity = .2
plot_material.use_shadows = True
plot_material.use_transparent_shadows = True
plot_material.use_raytrace = True
plot_material.use_transparency = True
plot_material.transparency_method = 'RAYTRACE'
plot_material.alpha = .95
plot_material.specular_alpha = 1
plot_material.raytrace_transparency.depth = 5
plot_material.use_vertex_color_paint = True

text_font = bpy.data.fonts.load(os.path.abspath(os.path.expanduser(font)))

def heatmap(heat):
    """Heat map: given a "heat" between 0 and 1, return a tuple of RGB
    values."""
    r = np.max((2*heat-1., 0))
    b = np.max((1.-2*heat, 0))
    return (r, 1.-r-b, b)

def zheat(z, zmin, zmax, **kwargs):
    """Colour a vertex based on its z-height compared to the minimum and
    maximum z-values that occur."""
    return heatmap((z-zmin)/(zmax-zmin if zmin != zmax else 1))

def plot_function(x, y, func, auto_axes = True, xmarks=None,
        ymarks = None, zmarks = None, labels = None, thickness = 0.025,
        text_rot = None, colourfunc=zheat, zmin = None, zmax = None):
    """Plot the function (lambda) func of x and y. The resulting surface is
    smooth-shaded. Vertices may be coloured according to colourfunc: this is a
    function that accepts the following parameters and returns an RGB-tuple:
        x, y, z, xmin, xmax, ymin, ymax, zmin, zmax"""
    global obj_ind, plot_id

    ids = {
        'axes': [],
        'axis_labels': [],
        'xmarks': [],
        'ymarks': [],
        'zmarks': [],
        'xlabels': [],
        'ylabels': [],
        'zlabels': []
    }

    if text_rot is None:
        text_rot = np.array((0, 0, 0))

    if plot_id is None:
        obj_id = 'plot_{}'.format(obj_ind)
        obj_ind += 1
        # Generate all vertices in the plot at z = 0
        verts = [(i, j, 0) for i in x for j in y]
        faces = []
        count = 0
        # Build faces from the vertices
        for i in range(len(y)*(len(x)-1)):
            if count < len(y)-1:
                faces.append((i, i+1, i+len(y)+1, i+len(y)))
                count += 1
            else:
                count = 0

        # Create a mesh and an object at the origin
        mesh = bpy.data.meshes.new(obj_id)
        obj = bpy.data.objects.new(obj_id, mesh)
        obj.location = (0, 0, 0)
        bpy.context.scene.objects.link(obj)
        mesh.from_pydata(verts, [], faces)
        mesh.update(calc_edges=True)

        # Create a new vertex colour map
        colours = obj.data.vertex_colors.new()

        # Set material
        obj.data.materials.append(plot_material)

        # Smooth-shade polygons
        for pol in obj.data.polygons:
            pol.use_smooth = True
    else:
        obj = bpy.data.objects[plot_id]
        colours = obj.data.vertex_colors.active

    verts = obj.data.vertices

    # Move vertices to their correct position
    for v in verts:
        v.co.z = func(v.co.x, v.co.y)
    obj.data.update(calc_edges=True)

    sv = sorted([(v.co.x, v.co.y, v.co.z) for v in verts], key=lambda q: q[2])

    # Colour vertices
    for pol in obj.data.polygons:
        for idx in pol.loop_indices:
            co = obj.data.vertices[obj.data.loops[idx].vertex_index].co
            colours.data[idx].color = colourfunc(x=co.x, y=co.y, z=co.z,
                xmin=np.min(x), xmax=np.max(x),
                ymin=np.min(y), ymax=np.max(y),
                zmin=sv[0][2], zmax=sv[-1][2])

    if auto_axes and plot_id is None:
        # Axes
        ids['axes'].append(add_line(np.array((min(x), min(y), 0)),
            np.array((max(x), min(y), 0)), thickness, False))
        ids['axes'].append(add_line(np.array((max(x), min(y), 0)),
            np.array((max(x), max(y), 0)), thickness, False))
        ids['axes'].append(add_line(
            np.array((min(x), min(y), sv[0][2] if zmin is None else zmin)),
            np.array((min(x), min(y), sv[-1][2] if zmax is None else zmax)),
            thickness, False))
        # Axis marks
        if xmarks is not None:
            for pos, label in xmarks:
                p = np.array((pos, min(y), 0))
                ids['xmarks'].append(add_line(p,
                    p-np.array((0, 1.5*thickness, 0)), thickness, False))
                if label is not None and len(label) > 0:
                    ids['xlabels'].append(add_text(
                        p-np.array((0, 7*thickness, 0)), label,
                        thickness, text_rot))
        if ymarks is not None:
            for pos, label in ymarks:
                p = np.array((max(x), pos, 0))
                ids['ymarks'].append(add_line(p,
                    p+np.array((1.5*thickness, 0, 0)), thickness, False))
                if label is not None and len(label) > 0:
                    ids[ 'ylabels'].append(add_text(
                        p+np.array((7*thickness, 0, 0)), label,
                        thickness, text_rot))
        if zmarks is not None:
            for pos, label in zmarks:
                p = np.array((min(x), min(y), pos))
                ids['zmarks'].append(add_line(p,
                    p-np.array((1.5*thickness/np.sqrt(2),
                        1.5*thickness/np.sqrt(2), 0)), thickness, False))
                if label is not None and len(label) > 0:
                    ids['zlabels'].append(add_text(
                        p-np.array((7*thickness, 7*thickness, 0))/np.sqrt(2),
                        label, thickness, text_rot))

        # Axis labels
        if labels is not None:
            ids['axis_labels'].append(add_text(
                np.array((max(x)+8*thickness, min(y), 0)),
                labels[0], 2*thickness, text_rot))
            ids['axis_labels'].append(add_text(
                np.array((max(x), max(y)+8*thickness, 0)),
                labels[1], 2*thickness, text_rot))
            ids['axis_labels'].append(add_text(
                np.array((min(x), min(y),
                    (sv[-1][2] if zmax is None else zmax) + 8*thickness)),
                labels[2], 2*thickness, text_rot))

    if plot_id is None:
        plot_id = obj_id

    ids['plot'] = plot_id

    return ids

def add_text(r, text, size=0.025, rotation=None):
    """Add text at the position r. Size is a relative parameter; use
    trial-and-error here."""
    global obj_ind
    obj_id = 'text_{}'.format(obj_ind)
    obj_ind += 1
    rot = [np.pi/2, 0, 0] if rotation is None else rotation.tolist()
    bpy.ops.object.text_add(location=r.tolist(), rotation=rot)
    obj = bpy.context.active_object
    obj.name = obj_id
    obj.data.name = obj_id
    obj.data.body = text

    # Set the font
    obj.data.font = text_font

    obj.data.offset_x = -2*size
    obj.data.offset_y = -2*size
    obj.data.shear = 0.0
    obj.data.size = 8*size
    obj.data.space_character = 1
    obj.data.space_word = 4*size
    obj.data.extrude = size/3

    obj.data.materials.append(text_material)

    return obj_id

def add_line(r1, r2, w=0.01, rel_w=True):
    """Add a "line" (cylinder) between the points r1 and r2. The width is
    either w (if rel_w is False) or w*|r2-r1| (if rel_w is True)."""
    global obj_ind
    obj_id = 'line_{}'.format(obj_ind)
    obj_ind += 1
    rc = (r2+r1)/2 # Centroid
    rr = r2-rc # Position of r2 relative to centroid
    r = np.sqrt(np.sum(rr**2))
    theta = np.arccos(rr[2]/r)
    phi = np.arctan2(rr[1], rr[0])
    bpy.ops.mesh.primitive_cylinder_add(vertices=16,
            radius=.5*w*(r if rel_w else 1), depth=2*r,
            location=rc.tolist(), rotation=(0, theta, phi))
    obj = bpy.context.active_object
    obj.name = obj_id
    for pol in obj.data.polygons:
        pol.use_smooth = True
    obj.data.materials.append(line_material)

    return obj_id
### End generic Blender rendering code

def frame_change(scene):
    """Update the plot for a given frame."""
    frame = min(max(scene.frame_current, 0), n_frames - 1)
    plot_function(x, k, funcgen(t[frame]), colourfunc=colgen(t[frame]))
    # Update the t-indicator
    bpy.data.objects[ttext_id].data.body = 't = {: >4.3f}'.format(t[frame])

if __name__ == '__main__':
    # Set up x, y and t data
    n_frames = 51
    nx = 101
    nk = 101
    xscale = 1/2
    kscale = 1/3
    zscale = 2
    x = np.linspace(0, 10, nx)*xscale
    k = np.linspace(0, 4*np.pi, nk)*kscale
    t = np.linspace(0, 10, n_frames)

    # Function to plot
    func = lambda x, k, t: np.exp(1j*(k*x-omega*t))*zscale
    # Generator for plottable function f(x, k) at time t
    funcgen = lambda t: lambda x, k: np.real(func(x, k, t))
    # Colour generator
    colgen = lambda t: lambda x, y, **kwargs: \
            heatmap((np.imag(func(x, y, t))+1)/2)

    # Absolute z range for axes
    azmin = -1*zscale
    azmax = 1*zscale

    # Axis marks
    xmarks = [(i*xscale, str(i)) for i in range(1, 11)]
    kmarks = [(j*np.pi/2*kscale, '{}π/2'.format(j if j != 1 else '') \
        if j % 2 == 1 else '{}π'.format(j // 2 if j != 2 else '')) \
        for j in range(1, 9)]
    zmarks = [(z/10*zscale, str(z/10)) for z in range(-10, 12, 2)]

    # Hide the 吸牛 splash screen
    bpy.context.user_preferences.view.show_splash = False

    # Remove existing meshes
    for item in bpy.context.scene.objects:
        if item.type == 'MESH':
            bpy.context.scene.objects.unlink(item)
    for item in bpy.data.objects:
        if item.type == 'MESH':
            bpy.data.objects.remove(item)
    for item in bpy.data.meshes:
        bpy.data.meshes.remove(item)

    # Set the camera position
    bpy.data.objects['Camera'].location = (11, -6, 5.5)
    bpy.data.objects['Camera'].rotation_euler = (1.1, 0, 0.8)

    # Let all texts face the camera
    rot = np.array(bpy.data.objects['Camera'].rotation_euler)

    # Initial t=0 plot; this also sets up the axes.
    print(plot_function(x, k, funcgen(0),
        True, labels=('x', 'k', 'z'), text_rot=rot, xmarks=xmarks,
        ymarks=kmarks, zmarks=zmarks, zmin=azmin, zmax=azmax,
        colourfunc=colgen(0)))

    # Text label indicating current time
    ttext_id = add_text(np.array((5.6, -1.2, 0)), 't = {: >4.2f}'.format(0),
        size=0.05, rotation=rot)

    # Set min/max/current frame
    bpy.data.scenes['Scene'].frame_start = 0
    bpy.data.scenes['Scene'].frame_end = n_frames - 1
    bpy.data.scenes['Scene'].frame_current = 0

    # Add frame change handler. This is what makes the animation happen!
    bpy.app.handlers.frame_change_pre.append(frame_change)

    # Add some environment lighting
    wld = bpy.data.worlds['World']
    wld.light_settings.use_environment_light = True
    wld.light_settings.environment_energy = .5

    # Add a white backdrop plane
    plane_material = bpy.data.materials.new('backdrop')
    plane_material.diffuse_color = (1, 1, 1)
    plane_material.use_shadeless = True
    bpy.ops.mesh.primitive_plane_add(location=(0, 0, -5))
    bpy.context.active_object.scale = (50, 50, 0)
    bpy.context.active_object.data.materials.append(plane_material)




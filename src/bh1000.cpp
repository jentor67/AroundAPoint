#include<stdio.h>
#include<fstream>
#include<iostream>
#include<string>
#include<sstream>
#include<vector>
#include<cstdlib>
#include<math.h>
#include<ctime>
#include<unistd.h>
#include "constants.hpp"
#include "orbitalParameters.hpp"
#include "startParameters.hpp"
#include "structures.hpp"
#include "vectors.hpp"

//*** Compi and run ***
//Program bh1000.cpp
//Purpose: Create 1000 random black holes around SagA
//observing the orbital effects
//compile:
//>./compile
//
//run:
//>./bh1000.o
//**********************

using namespace std;


int main(void)
{

    double mass[objects]; // mass array
    double i, omega, OMEGA, nue, a, mue, ra, rp, e, b, T;
    double x, y, z, u, v, w;
    int ct;
    long timet;

    clock_t begin = clock();
 
    BlackHole bh[objects]; // Declaring the structure

    ct = 0;
    bh[ct].mass = mSagA;
    bh[ct].x = 0;
    bh[ct].y = 0;
    bh[ct].z = 0;
    bh[ct].u = 0;
    bh[ct].v = 0;
    bh[ct].w = 0;
    ct += 1;
    bh[ct].mass = randomMass(5, 15);
    i = randomInclination();
    omega = randomArgumentOfPeriapsis();
    OMEGA = randomLongitudeOfAscendingNode();
    nue = randomTrueAnomaly();
    e = randomEccentricity();
    a = randomSimiMajorAxis(.01, .02);
    b = a*pow((1-pow(e,2)),.5);
    radiusVelocity(bh[ct].mass, a, e, i, OMEGA, omega, 
        &rp, &ra, &mue, &T);

    startPointVelocity(a, e, nue, rp, omega, i, OMEGA, mue, b, 
        &x, &y, &z, &u, &v, &w);
    bh[ct].x = x;
    bh[ct].y = y;
    bh[ct].z = z;
    bh[ct].u = u;
    bh[ct].v = v;
    bh[ct].w = w;
    bh[ct].T = T;
    ct += 1;

    while( ct < objects ){
        bh[ct].mass = randomMass(1,5);
        i = randomInclination();
        omega = randomArgumentOfPeriapsis();
        OMEGA = randomLongitudeOfAscendingNode();
        nue = randomTrueAnomaly();
        e = randomEccentricity();
        a = randomSimiMajorAxis(.01, .02);
        b = a*pow((1-pow(e,2)),.5);
        radiusVelocity(bh[ct].mass, a, e, i, OMEGA, omega, 
            &rp, &ra, &mue, &T);

        startPointVelocity(a, e, nue, rp, omega, i, OMEGA, mue, b, 
            &x, &y, &z, &u, &v, &w);
        bh[ct].x = x;
        bh[ct].y = y;
        bh[ct].z = z;
        bh[ct].u = u;
        bh[ct].v = v;
        bh[ct].w = w;
        bh[ct].T = T;

        ct += 1;
    }

    // *****Create files*****
    int fileN, msol;


    //  *** delete files in temp directory **
    //system("exec rm -r /home/jmajor/temp/*");
    //cout << get_current_dir_name() << '\n';
    stringstream call_line;
    call_line << "exec rm -r " << get_current_dir_name() << "/objects/object*";
    system(call_line.str().c_str());

    string filename[objects];
  
    // ***Create file names*** 
    fileN=0;
    while( fileN < objects )
    {
        filename[fileN] = get_current_dir_name();
        filename[fileN].append("/objects/object");
        stringstream ss;
        ss << fileN;
        string out_string;
        out_string = ss.str();
        int str_length = out_string.length();
        for ( int iPad = 0; iPad < 5 - str_length; iPad++)
          filename[fileN].append("0");
        filename[fileN].append(out_string);
        filename[fileN].append("-");
        msol = round(100*bh[fileN].mass/SOLARMASS);
        //cout << msol << '\n';
        ss.clear(); //clear any bits set
        ss.str(std::string());
        ss << msol;
        out_string = ss.str();
        filename[fileN].append(out_string);
        filename[fileN].append(".txt");
        cout << filename[fileN] << '\n';
        fileN += 1;
    }


    //   ***Open files***
    ofstream outputFile[objects];
    fileN = 0;
    while( fileN < objects )
    {
        //cout << "Opening file: " << filename[fileN] << '\n';
        outputFile[fileN].open(filename[fileN]);
        fileN += 1;
    }
 

    timet=0;
    while(timet < TIMELIMIT)
    {
        fileN=0;
        while(fileN < objects)
        {
            outputFile[fileN] << timet << " " << bh[fileN].x << " " 
              << bh[fileN].y << " " << bh[fileN].z << " " << 
              bh[fileN].u << " " << bh[fileN].v << " " << 
              bh[fileN].w << '\n';
            fileN += 1;
        }
        //cout << (TIMELIMIT - timet)  << '\n';

        stepMotion(bh);
        cout << timet << " " << bh[3].x << " " << bh[3].y << " " << 
          bh[3].z << '\n';
        
        
        timet += TIMESTEP;
    }
    clock_t end = clock();
    double elapsed_secs = double(end - begin) / CLOCKS_PER_SEC;
    cout << elapsed_secs << '\n';

    cout << "Closing file\n";

    fileN=0;
    while( fileN < objects )
    {
        outputFile[fileN].close();
        fileN += 1;
    }

}

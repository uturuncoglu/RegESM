//=======================================================================
// Regional Earth System Model (RegESM)
// Copyright (c) 2013-2019 Ufuk Turuncoglu
// Licensed under the MIT License.
//=======================================================================
#ifndef __GRID_H__
#define __GRID_H__

#include "vtkSmartPointer.h"

class vtkMultiBlockDataSet;
class vtkUnstructuredGrid;
class vtkStructuredGrid;
class vtkCPDataDescription;
class vtkDataObject;

namespace ESMFAdaptor {

  enum GridType {
    RECTILINEAR
  };

  template<GridType gridType>
  class Grid {
    private:
      // name
      char* GridName;

      // MPI rank
      int MPIRank;

      // total number of MPI processes
      int NProc;

      // total number of points on a MPI process
      int NPoints;

      // total number of cells on a MPI process
      int NCells;

      // size of each dimension (global)
      int NLon, NLat, NLev;

      // bounds in each MPI process
      int IndLonL, IndLonU;
      int IndLatL, IndLatU;
      int IndLevL, IndLevU;  

      // dimension data of each MPI process
      float* Lon;
      float* Lat;
      float* Lev;

      // grid
      vtkSmartPointer<vtkMultiBlockDataSet> grid;

    private:
      // creates a grid
      vtkSmartPointer<vtkMultiBlockDataSet> CreateGrid(const char* name);

    public:
      // constructor and deconstructor
      Grid();
     ~Grid();

      // creates a grid
      void Create(const char* name);

      // sets name
      void SetName(const char* name);
      char* GetName();

      // sets MPI rank
      void SetNProc(int nproc);

      // sets MPI rank
      void SetMPIRank(int rank);

      // sets total number of points on a MPI processor
      void SetNPoints(int ncells);
      int GetNPoints();
      
      // sets total number of cells on a MPI processor
      void SetNCells(int ncells);
      int GetNCells();

      void SetBounds(int* lb, int* ub);

      // sets coordinate variables for grid
      void SetLon(int nlon, int ncells, float* lon);
      void SetLat(int nlat, int ncells, float* lat);
      void SetLev(int nlev, int ncells, float* lev);

      vtkSmartPointer<vtkMultiBlockDataSet> GetGrid() const;
     
      // link grid with coprocessor data
      static bool SetToCoprocessor(vtkCPDataDescription* coprocessorData, const char* name, int* dims, vtkSmartPointer<vtkMultiBlockDataSet> grid);

      // update data
      static void SetAttributeValue(vtkCPDataDescription* coprocessorData, float* data, const char* vname, const char* pname, int* size, int* mpiSize, int* mpiRank);
  };
};

#endif

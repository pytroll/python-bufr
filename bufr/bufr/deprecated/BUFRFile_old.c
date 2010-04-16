/*
 * Python wrapper for reading BUFR files incrementally 
 *
 * Author Kristian Rune Larsen <krl@dmi.dk>
 *
 *
 *
 */


#include <stdio.h> 
#include <stdlib.h>
#include "Python.h"
#include "numpy/arrayobject.h"
#include "structmember.h"


/* BUFR fill values for int and double precision*/

#define IFILL 2147483647
#define DFILL 1.7e38

// Number of values and elements in the BUFR file. 
#define KVALS 360000
#define KELEM 40000

#define FREE_POINTER(x) { if(x != NULL) free(x); }

/* Helper functions */
int trim( char* str ) {

	/* 
	 * Get rid of trailing white-space characters, return number of removed characters
	 */

	char *p;
	int n = 0;
	size_t l = strlen( str );

	if ( l > 0 )
		for ( p = &str[l-1]; n < l && isspace( (int)*p ); --p, ++n )
			*p = '\0';
	return n;
};

void subst_space(char *str) {
	/* Creates a NC identifier from the bufr entry name by substituting
	   disallowed characters  with underscore. */

	int n = 0;
	while(str[n] != '\0'){
		if (str[n]  == ' ' || 
				str[n] == ')'  || 
				str[n] == '('  || 
				str[n] == '/'  || 
				str[n] == '*'  ||
				str[n] == '"'  ||
				str[n] == '\\')
			str[n] = '_';
		n++;
	}
}

int find_entry_type(double * data , int len) {
	/* Figure out type double or int */

	/* integer assumed if all values a below 100 times machine epsilon
	   ...epsilon is approx 1e-7 for single point precision (float) ... */

	int i;
	for(i=0;i<len;i++)
		if( (data[i] - floor(data[i])) > 10e-7)
			return PyArray_DOUBLE;
	return PyArray_INT;
}

int collapse(double * data , int count ) {
    /* Returns true if all data is identical */
    double val = data[0];
    int i;
    for(i=0; i< count; i++){

        if ( data[i] != val)
            return 0;
    }
    /* The values are all alike */
   return 1; 
}



/*
 * BUFR File entry models BUFR data subset 
 * */


typedef struct {
	/*
	 * Struct for holding data for each bufr element (scanline)
	 */
	PyObject_HEAD
	PyObject* name;     /* name */
	PyObject* unit;     /* unit */
	PyObject * data;  /* data array */

} BUFRFile_BUFRFileEntryObject;

static PyMemberDef BUFRFileEntry_Members[] = {
    {"name", T_OBJECT_EX, offsetof(BUFRFile_BUFRFileEntryObject, name), 0, "name"},
    {"unit", T_OBJECT_EX, offsetof(BUFRFile_BUFRFileEntryObject, unit), 0, "unit"},
    {"data", T_OBJECT_EX, offsetof(BUFRFile_BUFRFileEntryObject, data), 0, "data"},
    {NULL}  /* Sentinel */
};


static PyObject *BUFRFileEntry_new(PyTypeObject *type, PyObject *args, PyObject *kw) {
	/* Creates a new instance for a BUFRFile.BUFRFile object */
	BUFRFile_BUFRFileEntryObject *self = (BUFRFile_BUFRFileEntryObject *) type->tp_alloc(type, 0);
	self->name = NULL;
	self->unit = NULL;
	self->data = NULL;

	return (PyObject *) self;
}


static void BUFRFileEntry_dealloc(BUFRFile_BUFRFileEntryObject *self) {
	Py_XDECREF(self->data); 
	Py_XDECREF(self->name);
	Py_XDECREF(self->unit);
	self->ob_type->tp_free(self);
}

static int BUFRFileEntry_init(BUFRFile_BUFRFileEntryObject *self, PyObject *args, PyObject *kw) {

	PyObject * data;
	PyObject * name;
	PyObject * unit;

	if (!PyArg_ParseTuple(args, "OOO",&name, &unit, &data)) {
		return -1;
	}
	self->data = data;
	self->name = name;
	self->unit = unit;

	Py_XINCREF(self->data);
	Py_XINCREF(self->unit);
	Py_XINCREF(self->name);

	return 0;
}


static PyMethodDef BUFRFileEntry_Methods[] = {
	{NULL, NULL, 0, NULL },
};


static PyTypeObject BUFRFile_BUFRFileEntryType = {
		PyObject_HEAD_INIT(NULL)
			0,
		"BUFRFile.BUFRFileEntry",                /* char *tp_name; */
		sizeof(BUFRFile_BUFRFileEntryObject),          /* int tp_basicsize; */
		0,                        /* int tp_itemsize;*/
		(destructor) BUFRFileEntry_dealloc,         /* destructor tp_dealloc; */
		0,                        /* printfunc  tp_print;   */
		0,                        /* getattrfunc  tp_getattr; */
		0,                        /* setattrfunc  tp_setattr;  */
		0,                        /* cmpfunc  tp_compare;  */
		0,                        /* reprfunc  tp_repr;    */
		0,                        /* tp_as_number */
		0,                        /* PySequenceMethods *tp_as_sequence; */
		0,                        /* PyMappingMethods *tp_as_mapping; */
		0,                        /* hashfunc tp_hash;    */
		0,                        /* ternaryfunc tp_call;  */
		0,                        /* reprfunc tp_str;     */
		0,                        /* tp_getattro  */
		0,                        /* tp_setattro  */
		0,                        /* tp_as_buffer */
		Py_TPFLAGS_DEFAULT,      /* tp_flags */
		"BUFR file entry models BUFR data subsets ", /* tp_doc*/
		0,                        /* tp_traverse */
		0,                        /* tp_clear */
		0,                        /* tp_richcompare*/
		0,                        /* tp_weaklistoffset*/
		0,                        /* tp_iter*/
		0,                        /* tp_iternext */
		BUFRFileEntry_Methods,    /* tp_methods */
		BUFRFileEntry_Members,    /* tp_members */
		0,                        /* tp_getset*/
		0,                        /* tp_base */
		0,                        /* tp_dict */
		0,                        /* tp_descr_get */
		0,                        /* tp_descr_set */
		0,                        /* tp_dictoffset */
		(initproc) BUFRFileEntry_init,/* tp_init */
		0,                        /* tp_allow */
		BUFRFileEntry_new,        /* tp_new*/
		0,                        /* tp_free*/
	};


/*  
 * BUFRFile implementation below 
 *  */

typedef struct {
	PyObject_HEAD
    FILE * inpfp;
	char * filename[40];

	/* internal variables for reading BUFR subsection */
	char *bufr_message0;
	int *ksup, *ksec0, *ksec1, *ksec2, *ksec3, *ksec4, *key, *ktdlst,
	    *ktdexp;
	char *cnames, *cunits, *cvals;
	double *values;
	int Nbpw;
	int kelem, kvals, icode; /*number of elements, values, */

} BUFRFile_BUFRFileObject;


static PyObject *BUFRFile_new(PyTypeObject *type, PyObject *args, PyObject *kw) {
	/* Creates a new instance for a BUFRFile.BUFRFile object */
	BUFRFile_BUFRFileObject *self = (BUFRFile_BUFRFileObject *) type->tp_alloc(type, 0);
	self->inpfp = NULL;

	self->bufr_message0 = ( char* ) malloc(15000 *  sizeof(char));
	self->ksup = ( int* ) malloc(9 * sizeof(long int));
	self->ksec0 = ( int* ) malloc(3 * sizeof(int));
	self->ksec1 = ( int* ) malloc(40 *  sizeof(int));
	self->ksec2 = ( int* ) malloc(4096 * sizeof(int));
	self->ksec3 = ( int* ) malloc(4 * sizeof(int));
	self->ksec4 = ( int* ) malloc(2 * sizeof(int));
	self->key = ( int* ) malloc(46 * sizeof(int));
	self->ktdlst = ( int* ) malloc(KELEM * sizeof(int));
	self->ktdexp = ( int * ) malloc(KELEM * sizeof(int));
	/* variable values */
	self->values = ( double * ) malloc(KVALS * sizeof(double));
	/*cnames , variable names */
	self->cnames = ( char * ) malloc(KELEM * 64 * sizeof(char));
	/*cunits , variable units */
	self->cunits = ( char * ) malloc(KELEM*24 * sizeof(char));
	/*cvals , charater values ... not suppoted yet */
	self->cvals = (char*) malloc(KVALS*80 * sizeof(char)); 

	if (self->bufr_message0 == NULL ||
			self->ksup == NULL ||
			self->ksec0 == NULL ||
			self->ksec1 == NULL ||
			self->ksec2 == NULL ||
			self->ksec3 == NULL ||
			self->ksec4 == NULL ||
			self->key == NULL ||
			self->ktdlst == NULL ||
			self->ktdexp == NULL ||
			self->values == NULL ||
			self->cnames == NULL ||
			self->cunits == NULL ||
			self->cvals == NULL ) {
		/* Memoru allocation failed , raise exception ... I don't know if this is good 
		 * practice in a constructor ??? */
		PyErr_SetString(PyExc_Exception, "Memory allocation failed");
		return NULL;
	} 

	return (PyObject *) self;
}

static void BUFRFile_dealloc(BUFRFile_BUFRFileObject *self) {
	if(self->inpfp) {
		fclose(self->inpfp);
	}

	FREE_POINTER( self->bufr_message0 ) 
		FREE_POINTER( self->ksup ) 
		FREE_POINTER( self->ksec0 )
		FREE_POINTER( self->ksec1 )
		FREE_POINTER( self->ksec2 )
		FREE_POINTER( self->ksec3 )
		FREE_POINTER( self->ksec4 )
		FREE_POINTER( self->key )
		FREE_POINTER( self->ktdlst )
		FREE_POINTER( self->ktdexp )
		FREE_POINTER( self->values )
		FREE_POINTER( self->cnames ) 
		FREE_POINTER( self->cunits )
		FREE_POINTER( self->cvals )

		self->ob_type->tp_free(self);
}

static int BUFRFile_init(BUFRFile_BUFRFileObject *self, PyObject *args, PyObject *kw) {
	char * inp_filename;
	if (!PyArg_ParseTuple(args, "s", &inp_filename)) {
		return -1;
	}
	if(self->inpfp) {
		PyErr_SetString(PyExc_Exception, "__init__ already called");
		return -1;
	}
	if(sizeof(long) == 4) self->Nbpw=32;
	else if(sizeof(long) == 8) self->Nbpw=64;
	else{
		PyErr_SetString(PyExc_Exception, "Wrong architecture");
		return -1;
	}
	self->kelem = KELEM;
	self->kvals = KVALS;
	self->icode = 0;

	strcpy(self->filename, inp_filename);
	self->inpfp = fopen(inp_filename, "rb");
	if (self->inpfp == NULL) {
		PyErr_SetString(PyExc_Exception, "unable to open file ");
		return -1;
	}
	return 0;

}

static PyObject * BUFRFile_read(BUFRFile_BUFRFileObject *self) {

    if(!self->inpfp) {
		PyErr_SetString(PyExc_Exception, "File not open");
		return NULL;
    }

	/*********** Read in bufr messages ***********/
	int length = 15000;
	int kerr = 0;
	int * kbuff;
	int status = 0; /*status for reading one new entry */
	char* bufr_message;

	bufr_message = self->bufr_message0; /* make sure we alwas start at the
					       beginning of the message array
					       ... shouldn't be necessary. */

	/* read entry into buffer*/
	status = readbufr( self->inpfp, bufr_message, &length);

	if( status == -1 ) 
		return NULL;
	else if( status == -2 ) {
		PyErr_SetString(PyExc_Exception, "Error in file handling");
		return NULL;
	}   
	else if( status == -3 ) {
		PyErr_SetString(PyExc_Exception, "Too small input array");
		return NULL;
	}
	else if( status == -4 ) {
		PyErr_SetString(PyExc_Exception, "Too small input array");
		return NULL;
	}
	else if( status < 0 ) {
		PyErr_SetString(PyExc_Exception, "Error reading BUFR");
		return NULL;
	}
	/*    Expand bufr message calling fortran program */
	kbuff = (int *) bufr_message;
	length /= 4;

	/* Get BUFR subsection limits */
	bus012_(&length, kbuff , self->ksup, self->ksec0, self->ksec1, self->ksec2,  &kerr) ;

	if (self->ksup[5] > 1)
		self->kelem = self->kvals/self->ksup[5];
	else
		self->kelem = KELEM;

	if ( self->kelem > KELEM ) self->kelem = KELEM;
	kerr = 0;
	bufrex_(&length,( long * ) kbuff,self->ksup,self->ksec0,self->ksec1,self->ksec2,self->ksec3,self->ksec4,
			&(self->kelem), self->cnames, self->cunits ,&(self->kvals),
			self->values, self->cvals,&kerr);


	/* find number of subsets = number of meassurements */
	int nsup = self->ksup[5];

    /* since we are comming from shitty fortran we need to rotate the values , in order for it to make sense in C*/
    double * tvalues;
    tvalues = ( double * ) malloc(KVALS * sizeof(double));

    int cr,cc;
    for(cr=0; cr < self->kelem ; cr++)
        for(cc=0; cc< nsup; cc++)
            *(tvalues + cr*nsup + cc) = *(self->values + cc*self->kelem + cr);
    double * values_old = self->values;
    self->values = tvalues;
    FREE_POINTER(values_old);

	if ( kerr )
	{
		PyErr_SetString(PyExc_Exception, "Error reading BUFR entry");
		return NULL;
	}

	int s,r;

	/*
	 * run through bufr names, units and values and insert data info
	 * bufr_entry structs. 
	 *
	 */

	char * mycname = (char *) malloc(65*sizeof(char));
	char * myunits = (char *) malloc(25*sizeof(char));

	/*Here we need a python list instead ... ! */
	PyObject * bufr_entries = PyList_New(0);

	char ch;

	for(s=0; s < self->kelem; s++ ){
		/* get name of this entry and trim whitespace*/
		for(r=0; r< 64; r++){
			ch = tolower(*(self->cnames + s*64 + r));
			mycname[r] = ch;
		}

		mycname[64] = '\0';
		trim(mycname);

		/* get name of unit of this entry and trim whitespace*/
		for(r=0; r<24; r++) {
			ch = tolower(*(self->cunits + s*24 + r));
			myunits[r] = ch;
		}
		myunits[24] = '\0'; /*insert eol missing from fortran
				      string */
		trim(myunits); /* trims white space from end of string */

		/* Don't accept empty names*/
		if(mycname[0] == '\0')
			continue;

		subst_space(mycname); /*make a valid name*/

		/* Create a New BUFRFileEntry object to hold the data */
		PyObject *bufr_entry_args, *bufr_entry, *name, *unit;

		/* unit string*/
		unit = PyString_FromFormat("%s", myunits);
		/* name string */
		name = PyString_FromFormat("%s_%d", mycname, s);

		/* subsets differ by kelem = number of elements. Notice
		 * that the fill values are from the
		 * bufr definition.
		 * mapping:
		 * missing BUFR int = 2147483647
		 * missing BUFR real = 1.7E38
		 *
		 */

		/* Create data array */
		int dimensions[1];
		dimensions[0]=nsup;
        PyObject * bdata;
        if( collapse(self->values + s*nsup, nsup) )
            bdata = PyFloat_FromDouble( *(self->values+s*nsup) );
        else
            bdata = (PyArrayObject *) PyArray_FromDimsAndData(1, dimensions,PyArray_DOUBLE, (char*) (self->values + s*nsup));

        if (bdata == NULL) {
            PyErr_SetString(PyExc_Exception, "Unable to retrieve BUFR data");
            return NULL;
        }

		/* Create bufr enty object and insert object into list */
		bufr_entry_args = PyTuple_New(3);
		PyTuple_SetItem(bufr_entry_args,0,name);
        if(unit)
		    PyTuple_SetItem(bufr_entry_args,1, unit);
        else
            PyTuple_SetItem(bufr_entry_args,1, Py_None);
		PyTuple_SetItem(bufr_entry_args,2,bdata);

		bufr_entry =  PyObject_CallObject((PyObject *) &BUFRFile_BUFRFileEntryType, bufr_entry_args);
        Py_DECREF( bufr_entry_args );

		if (PyList_Append(bufr_entries,bufr_entry)) {
			PyErr_SetString(PyExc_Exception, "Error reading BUFR entry");
			return NULL;
		}

	}

	/* exit define mode .. start to fill in values */
	free(mycname );
	free(myunits );

	return bufr_entries;

} // End of netcdf define

static PyObject * BUFRFile_close(BUFRFile_BUFRFileObject *self) {
	if (! self->inpfp ) {
		PyErr_SetString(PyExc_Exception, "BUFRFile not open");
		return NULL;
	}
    fclose(self->inpfp);
    Py_RETURN_NONE;
}

static PyObject * BUFRFile_iter(BUFRFile_BUFRFileObject *self) {
	return self;
}
static PyObject * BUFRFile_next(BUFRFile_BUFRFileObject *self) {
    if(!self->inpfp) {
        PyErr_SetString(PyExc_Exception,"BUFRFile not open");
        return NULL;
    }
	PyObject * res; 
    do {
        res = BUFRFile_read(self);
        if (!res) {
            PyErr_SetString(PyExc_StopIteration, "BUFRFile not open");
            return NULL;
        }
        return res;
    }
    while(res != NULL);

    PyErr_SetString(PyExc_StopIteration, "BUFRFile not open");
    return NULL;
}

static PyObject * BUFRFile_reset(BUFRFile_BUFRFileObject *self) {
    BUFRFile_close(self);
	self->inpfp = fopen(self->filename, "rb");
    if (!self->inpfp) {
        PyErr_SetString(PyExc_Exception, "Unable to reset file");
        return NULL;
    }
    Py_RETURN_NONE;
}


static PyObject * BUFRFile_keys(BUFRFile_BUFRFileObject *self) {
    if(!BUFRFile_reset(self))
        return NULL;
    PyObject * bufr_entries = BUFRFile_next(self);
    if(!bufr_entries ) {
        BUFRFile_reset(self);
        return NULL;
    }
    
	PyObject * bufr_keys = PyList_New(0);
    if(!bufr_keys ) {
        PyErr_SetString(PyExc_Exception, "Error creating keys list");
        BUFRFile_reset(self);
        return NULL;
    }
    int i;
    for(i=0; i<PyList_Size(bufr_entries); i++) {
        BUFRFile_BUFRFileEntryObject * entry;
        entry = (BUFRFile_BUFRFileEntryObject *) PyList_GetItem(bufr_entries, i);
        if (PyList_Append(bufr_keys,entry->name)) {
            PyErr_SetString(PyExc_Exception, "Error getting keys");
            BUFRFile_reset(self);
            return NULL;
        }
    }
    /* Reset filepointer before returning */
    BUFRFile_reset(self);
    return bufr_keys;
}

static PyMemberDef BUFRFile_Members[] = {
    {"filename", T_CHAR, offsetof(BUFRFile_BUFRFileObject, filename), 0, "filname"},
    {NULL}  /* Sentinel */
};

static PyMethodDef BUFRFile_Methods[] = {
	{ "read", BUFRFile_read, METH_NOARGS, "Reads a subset of the BUFR file" }, 
	{ "close", BUFRFile_close, METH_NOARGS, "Closes input BUFR file"},
	{ "next", BUFRFile_next, METH_NOARGS, "Reads next subset, for python iterator."},
	{ "keys", BUFRFile_keys, METH_NOARGS, "Returns a list of the BUFR file variables."},
	{ "reset", BUFRFile_reset, METH_NOARGS, "Resets the BUFR file pointer to initial position."},
	{NULL, NULL, 0, NULL }
};


static PyTypeObject BUFRFile_BUFRFileType = {
	PyObject_HEAD_INIT(NULL)
	0,
	"BUFRFile.BUFRFile",                /* char *tp_name; */
	sizeof(BUFRFile_BUFRFileObject),          /* int tp_basicsize; */
    0,                        /* int tp_itemsize;*/
    (destructor) BUFRFile_dealloc,         /* destructor tp_dealloc; */
    0,                        /* printfunc  tp_print;   */
    0,                        /* getattrfunc  tp_getattr; */
    0,                        /* setattrfunc  tp_setattr;  */
    0,                        /* cmpfunc  tp_compare;  */
    0,                        /* reprfunc  tp_repr;    */
    0,                        /* tp_as_number */
    0,                        /* PySequenceMethods *tp_as_sequence; */
    0,                        /* PyMappingMethods *tp_as_mapping; */
    0,                        /* hashfunc tp_hash;    */
    0,                        /* ternaryfunc tp_call;  */
    0,                        /* reprfunc tp_str;     */
    0,                        /* tp_getattro  */
    0,                        /* tp_setattro  */
    0,                        /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT,      /* tp_flags */
    "BUFR file object supports reading of BUFR data subsets ", /* tp_doc*/
    0,                        /* tp_traverse */
    0,                        /* tp_clear */
    0,                        /* tp_richcompare*/
    0,                        /* tp_weaklistoffset*/
    BUFRFile_iter,            /* tp_iter*/
    BUFRFile_next,            /* tp_iternext */
    BUFRFile_Methods,         /* tp_methods */
    BUFRFile_Members,         /* tp_members */
    0,                        /* tp_getset*/
    0,                        /* tp_base */
    0,                        /* tp_dict */
    0,                        /* tp_descr_get */
    0,                        /* tp_descr_set */
    0,                        /* tp_dictoffset */
    (initproc) BUFRFile_init,           /* tp_init */
    0,                        /* tp_allow */
    BUFRFile_new,             /* tp_new*/
    0,                        /* tp_free*/
};


PyMODINIT_FUNC initBUFRFile() {
    PyObject *m;
    if (PyType_Ready(&BUFRFile_BUFRFileType) < 0 ) {
        return;
    }
    if (PyType_Ready(&BUFRFile_BUFRFileEntryType) < 0 ) {
        return;
    }
    m = Py_InitModule3("BUFRFile", BUFRFile_Methods, "Python BUFR file module.");
    import_array();
    Py_INCREF(&BUFRFile_BUFRFileType);
    Py_INCREF(&BUFRFile_BUFRFileEntryType);
    PyModule_AddObject(m, "BUFRFile", (PyObject *) &BUFRFile_BUFRFileType);
    PyModule_AddObject(m, "BUFRFileEntry", (PyObject *) &BUFRFile_BUFRFileEntryType);
}


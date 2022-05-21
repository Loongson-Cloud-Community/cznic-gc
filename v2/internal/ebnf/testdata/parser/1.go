package p

var a = []int{{{{42}}}}

// Key           = Expression .

//		not using follow sets			using follow sets
//	------------------------------			---------------------
//		enter	reject	accept			enter	reject	accept
//	1	  533	   407	   128			  181	    53	   128
//	2	  858	   712	   146			  206	    60	   146
//	3	 1181	  1017	   164			  230	    67	   164
//	4	 1504	  1322	   182			  256	    74	   182


// Key           = Expression | LiteralValue1 .

//		not using follow sets			using follow sets
//	------------------------------			---------------------
//		enter	reject	accept			enter	reject	accept
//	1	  536	   407	   129			  182	    53	   129
//	2	 1018	   813	   205			  287	    83	   205
//	3	 1982	  1625	   357			  500	   143	   357
//	4	 3910	  3249	   661			  923	   263	   661

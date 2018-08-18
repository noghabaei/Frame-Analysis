
C     **************************************************************** 
C     PROGRAM FOR THE SOLUTION OF 2D FRAME
C     ****************************************************************
	
	PROGRAM FRAME
      IMPLICIT REAL*8(A-H,O-Z)
           COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
     
C                                                                       
C     THIS ROUTINE OPEN THE FILES AND CALL DATA SUBROUTINE                      
C                                                     
     
      CHARACTER*6 TITLE(12)                                             

      OPEN(5,FILE='D:\FRAME.TXT',STATUS='UNKNOWN'
     .,ACCESS='SEQUENTIAL',FORM='FORMATTED')   
      
      OPEN(6,FILE='D:\FRAME_RESULT.TXT',STATUS='UNKNOWN',
     .ACCESS='SEQUENTIAL',FORM='FORMATTED')                            

      IFLAG=1
	CALL DATA
          
      
      
      IF (NDOFN.EQ.1) THEN
      CALL STIFFA
      CALL TRUSSLOAD
      CALL ASSEMB                                                     
      CALL GREDUC                                                      
      CALL BAKSUB                                                     
      CALL FORCE                                                       
      CALL RESULT
      
      
      END IF
      
      IF (NDOFN.EQ.2) THEN 
      CALL STIFFB
      CALL TRUSSLOAD
      CALL ASSEMB                                                     
      CALL GREDUC                                                      
      CALL BAKSUB                                                     
      CALL FORCE                                                       
      CALL RESULT
      END IF 
      
      

      
      IF (NDOFN.EQ.3)THEN
      IF (IFLAG.EQ.1) CALL STIFFC
      DO 27 K3=1,3
      CALL FRAMELOAD
      CALL ASSEMB                                                     
      CALL GREDUC                                                      
      CALL BAKSUB                                                     
      CALL FORCE                                                       
      CALL RESULT
      CALL SORT
   27 CONTINUE
      END IF
      
      OPEN(1,STATUS='UNKNOWN',ACCESS='SEQUENTIAL',FORM='FORMATTED')     

      CLOSE (1)                                                         
      CLOSE(5)                                                        
      CLOSE(6)                                                          
      STOP                                                             
      END                                                             

C     ****************************************************************   
C         THIS SUBROUTINE READS & WRITES USER INFO ABOUT STRUCTURE     
C     ****************************************************************    

C     SPECIFICATION PART 
     
      SUBROUTINE DATA                                                   
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ICODE(3),PRESC(3)                                     
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  

C	THESE SPECIFICATIONS ARE FOR DYNAMIC DIMENSIONING
      
C
C     READ AND WRITE THE PROBLEM TITLE                                  
C                                                                      
      READ(5,915) TITLE                                                 
      WRITE(6,915) TITLE                                           
  915 FORMAT(12A6)                                                     
C                                                                       
C     READ AND WRITE THE CONTROL DATA                                   
C                                                                       
      READ(5,*)  NPOIN,NELEM,NBOUN,NMATS,NPROP,NNODE,NDOFN,NDIME,       
     .             NSTRE                                               
  900 FORMAT(16I5)                                                    
      WRITE(6,905) NPOIN,NELEM,NBOUN,NMATS,NPROP,NNODE,NDOFN,NDIME,    
     .             NSTRE                                              
  905 FORMAT(//1X,7HNPOIN =,I5,3X,7HNELEM =,I5,3X,7HNBOUN =,I5,3X,      
     . 7HNMATS =,I5,//1X,7HNPROP =,I5,3X,7HNNODE =,I5,3X,             
     . 7HNDOFN =,I5,3X,7HNDIME =I5,//1X,7HNSTRE =,I5)                  
      NSVAB=NPOIN*NDOFN                                                
      NEVAB=NNODE*NDOFN                                                 
C                                                                      
C     READ AND WRITE THE MATERIAL PROPERTIES                          
C                                                                      
      WRITE(6,950)                                                     
  950 FORMAT(/,1H0,5X,19HMATERIAL PROPERTIES)                         
      DO 10 IMATS=1,NMATS                                             
      READ(5,*)  JMATS,(PROPS(JMATS,IPROP),IPROP=1,NPROP)              
   10 WRITE(6,910) JMATS,(PROPS(JMATS,IPROP),IPROP=1,NPROP)           
  910 FORMAT(I10,4F15.5)                                             
C                                                                      
C     READ AND WRITE THE ELEMENT(MEMBER) NODAL                        
C     (JOINT) CONNECTIONS                                              
C                                                                    
      WRITE(6,960)                                                     
  960 FORMAT(/,1H0,2X,2HEL,3X,5HNODES,3X,4HMAT.)                       
      DO 20 IELEM=1,NELEM                                             
      READ(5,*)  JELEM,(LNODS(JELEM,INODE),INODE=1,NNODE),MATNO(JELEM) 
   20 WRITE(6,920) JELEM,(LNODS(JELEM,INODE),INODE=1,NNODE),MATNO(JELEM)
  920 FORMAT(5I5)                                                       
C                                                                      
C     READ AND WRITE THE NODAL(JOINT) COORDINATES                      
C                                                                     
      WRITE(6,970)                                                     
  970 FORMAT(/,1H0,5X,4HNODE,5X,6HCOORD.)                             
      DO 30 IPOIN=1,NPOIN                                             
      READ(5,*)  JPOIN,(COORD(JPOIN,IDIME),IDIME=1,NDIME)            
   30 WRITE(6,930) JPOIN,(COORD(JPOIN,IDIME),IDIME=1,NDIME)          
  930 FORMAT(I10,3F15.5)                                             
C                                                                     
C     READ AND WRITE THE BOUNDARY CONDITIONS                          
C     AND STORE IN GLOBAL VECTORS                                     
C                                                                     
      DO 40 ISVAB=1,NSVAB                                              
      IFPRE(ISVAB)=0                                                 
   40 FIXED(ISVAB)=0.0                                                
      WRITE(6,980)                                                     
  980 FORMAT(/,1H0,1X,28HRESTRAINED NODES,FIXITY CODE,                 
     . 22H AND PRESCRIBED VALUES)                                    
      IF(NBOUN.EQ.0) GO TO 55                                         
      DO 50 IBOUN=1,NBOUN                                             

C     BOUNDRY DATA IS READ AS FOLLOWS :
C	   1-NUMBER OF NODE
C	   2-ICODE IN FIRST DEGREE OF FREEDOM FOR THE NODE (1 FOR FIXED 
C		 AND 0 FOR FREE RESTRAINT.	 
C	   3-THE MAGNITUDE OF PRESCIBED DISPLACEMENT
C	   THIS PROCESS IS DONE FOR EACH NODE WITH RESTRAINTS.

	READ(5,*)  NODFX,(ICODE(IDOFN),PRESC(IDOFN),IDOFN=1,NDOFN)        
      WRITE(6,*) NODFX,(ICODE(IDOFN),PRESC(IDOFN),IDOFN=1,NDOFN)      
C  940 FORMAT(I10,3(I5,F10.5))                                          
      DO 50 IDOFN=1,NDOFN                                             
      INDEX=(NODFX-1)*NDOFN+IDOFN                                      
      IFPRE(INDEX)=ICODE(IDOFN)                                       
   50 FIXED(INDEX)=PRESC(IDOFN)                                         
   55 CONTINUE                                                          
C                                                                      
C     READ AND WRITE THE NODAL(JOINT) APPLIED LOADS                   
C       
     
      
      RETURN                                                         
      END                      
C     ****************************************************************   
C         THIS SUBROUTINE READS & WRITES STRUCTURE LOADS     
C     ****************************************************************    

C     SPECIFICATION PART


      SUBROUTINE FRAMELOAD
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
      
      
            IF (K3.EQ.1) WRITE(6,*) "   FIRST NODAL LOAD "      
      IF (K3.EQ.2) WRITE(6,*) "   SECOND NODAL LOAD "
      IF (K3.EQ.3) WRITE(6,*) "   THIRD NODAL LOAD "
      
      
      WRITE(6,990)                                                     
  990 FORMAT(/,1H0,5X,4HNODE,7X,11HNODAL LOADS)                       
      DO 60 IPOIN=1,NPOIN                                              
      DO 60 IDOFN=1,NDOFN                                            
   60 RLOAD(IPOIN,IDOFN)=0.0                                           
   70 READ(5,*)  IPOIN,(RLOAD(IPOIN,IDOFN),IDOFN=1,NDOFN)              
      WRITE(6,930) IPOIN,(RLOAD(IPOIN,IDOFN),IDOFN=1,NDOFN)            
      IF(IPOIN.LT.NPOIN) GO TO 70
  930 FORMAT(I10,3F15.5)       
C     READ AND WRITE ELEMENTAL LOADS 

      

      IF (K3.EQ.1) WRITE(6,*) "   FIRST ELEMENTAL LOAD "      
      IF (K3.EQ.2) WRITE(6,*) "   SECOND ELEMENTAL LOAD "
      IF (K3.EQ.3) WRITE(6,*) "   THIRD ELEMENTAL LOAD "
      WRITE(6,*) "    WIDEFORCE,MOMENT,DISTANCE,FORCE,DISTANCE"
      DO 636 IELEMJ=1,NELEM
      
      READ(5,*)  IELEM ,RWIDEVALUE ,
     - RMOMENTVALUE,RMOMENTDIS,
     - RFORCEVALUE,RFORCEDIS
      WRITE(6,911) IELEM ,RWIDEVALUE ,
     - RMOMENTVALUE,RMOMENTDIS,
     - RFORCEVALUE,RFORCEDIS
 911  FORMAT(I10,6(F10.2))
      
      NODE1=LNODS(IELEM,1)                                              
      NODE2=LNODS(IELEM,2)                                              
      XPROJ=COORD(NODE2,1)-COORD(NODE1,1)                               
      YPROJ=COORD(NODE2,2)-COORD(NODE1,2)                             
      ELENG=SQRT(XPROJ*XPROJ+YPROJ*YPROJ) 
      SINTH=YPROJ/ELENG
      COSTH=XPROJ/ELENG
	  
	  
      ELOAD(IELEM,1)=((RFORCEVALUE*(ELENG-RFORCEDIS)/ELENG) +
     -(-6*RMOMENTVALUE*RMOMENTDIS*
     - (ELENG-RMOMENTDIS)/(ELENG*ELENG*ELENG))+
     -(RWIDEVALUE*ELENG/2))*SINTH
      
      ELOAD(IELEM,4)=((RFORCEVALUE*
     -((ELENG-RFORCEDIS)*(ELENG-RFORCEDIS))*RFORCEDIS/(ELENG*ELENG))+  
     -(RMOMENTVALUE*(ELENG-RMOMENTDIS)*(3*RMOMENTDIS-ELENG)/
     -(ELENG*ELENG))+
     -(RWIDEVALUE*ELENG*ELENG/12))*SINTH
      
      ELOAD(IELEM,2)=((RFORCEVALUE*(ELENG-RFORCEDIS)/ELENG) +
     -(-6*RMOMENTVALUE*RMOMENTDIS*
     - (ELENG-RMOMENTDIS)/(ELENG*ELENG*ELENG))+
     -(RWIDEVALUE*ELENG/2))*COSTH
      
      ELOAD(IELEM,5)=(RFORCEVALUE*RFORCEDIS/ELENG)+
     -(6*RMOMENTVALUE*RMOMENTDIS*
     - (ELENG-RMOMENTDIS)/(ELENG*ELENG*ELENG))+
     -(RWIDEVALUE*ELENG/2) 
      
      ELOAD(IELEM,3)=((RFORCEVALUE*
     -((ELENG-RFORCEDIS)*(ELENG-RFORCEDIS))*RFORCEDIS/(ELENG*ELENG))+  
     -(RMOMENTVALUE*(ELENG-RMOMENTDIS)*(3*RMOMENTDIS-ELENG)/
     -(ELENG*ELENG))+
     -(RWIDEVALUE*ELENG*ELENG/12))*COSTH
            
      
      
      ELOAD(IELEM,6)=(-RFORCEVALUE*(ELENG-RFORCEDIS)*
     -(RFORCEDIS*RFORCEDIS)/(ELENG*ELENG))+
     -(-RMOMENTVALUE*(RMOMENTDIS)*(3*(ELENG-RMOMENTDIS)-ELENG)/
     -(ELENG*ELENG))+
     -(-RWIDEVALUE*ELENG*ELENG/12)
      
 636  CONTINUE  
     
      
      
      RETURN 
      END
      
      
C     ***************************************************************
C     SUOUTINE STIFFB:IT CALCULATES THE LOCAL MATRIX FOR EACH ELEMENT
C                     WHEN NDOFN=2	                                                              
C     ***************************************************************	
      
       SUBROUTINE TRUSSLOAD
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300) 
      
   70 READ(5,*)  IPOIN,(RLOAD(IPOIN,IDOFN),IDOFN=1,NDOFN)              
      WRITE(6,930) IPOIN,(RLOAD(IPOIN,IDOFN),IDOFN=1,NDOFN)            
      IF(IPOIN.LT.NPOIN) GO TO 70
  930 FORMAT(I10,3F15.5)       
      
      
      
      
      RETURN                                                            
      END
      
      SUBROUTINE STIFFA
      IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION ESTIF(6,6)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)            
C                                                                       
C     EVALUATION OF MEMBER STIFFNESS MATRICES                           
C     FOR AXIAL BAR PRPOBLEMS                                  
C                                                                       
      REWIND 1                                                          
      DO 10 IELEM=1,NELEM                                               
      LPROP=MATNO(IELEM)                                                
      YOUNG=PROPS(LPROP,1)                                              
      XAREA=PROPS(LPROP,2)                                              
      NODE1=LNODS(IELEM,1)                                              
      NODE2=LNODS(IELEM,2)                                             
      ELENG=ABS(COORD(NODE2,1)-COORD(NODE1,1))                        
      FMULT=YOUNG*XAREA/ELENG                                           
      ESTIF(1,1)=FMULT                                    
      ESTIF(1,2)=-FMULT                                      
      ESTIF(2,1)=-FMULT                                      
      ESTIF(2,2)=FMULT									                        
      DO 100 I=1,2
      DO 100 J=1,2
  100 WRITE(1,*)ESTIF(I,J)   
   10 CONTINUE
	RETURN                                                            
      END
      
      
	SUBROUTINE STIFFB       
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ESTIF(6,6)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
         
C
C     EVALUATION OF MEMBER STIFFNESS MATRICES                           
C     FOR PIN JOINTED PLANE FRAMEWORKS                                                                                                      
      REWIND 1                                                          
      DO 20 IELEM=1,NELEM                                               
      LPROP=MATNO(IELEM)                                                
      YOUNG=PROPS(LPROP,1)                                              
      XAREA=PROPS(LPROP,2)                                              
      NODE1=LNODS(IELEM,1)                                              
      NODE2=LNODS(IELEM,2)                                          
	D1=COORD(NODE2,1)-COORD(NODE1,1)                                  
      D2=COORD(NODE2,2)-COORD(NODE1,2)                                  
      ELENG=SQRT(D1*D1+D2*D2)                                     
      SINTH=D2/ELENG                                                    
      COSTH=D1/ELENG                                                    
      FMULT=YOUNG*XAREA/ELENG                                           
      ESTIF(1,1)=FMULT*COSTH*COSTH                                      
      ESTIF(1,2)=FMULT*SINTH*COSTH                                      
      ESTIF(2,1)=FMULT*SINTH*COSTH                                      
      ESTIF(2,2)=FMULT*SINTH*SINTH									  
	DO 10 INODE=1,NNODE                                               
      DO 10 JNODE=1,NNODE                                               
      KOUNT=(-1)**INODE*(-1)**JNODE                                     
      DO 10 KNODE=1,NNODE                                               
      DO 10 LNODE=1,NNODE                                               
      INDEX=(INODE-1)*NNODE+KNODE                                       
      JNDEX=(JNODE-1)*NNODE+LNODE                                       
   10 ESTIF(INDEX,JNDEX)=KOUNT*ESTIF(KNODE,LNODE)                      
      DO 100 I=1,6
      DO 100 J=1,6
  100 WRITE(1,*)ESTIF(I,J)  
   20 CONTINUE
      
	RETURN                           
      END

      
      
C     ****************************************************************  
C         THIS SUBROUTINE READS & STORES ELEMENT STIFFNESS MATRICE
C     ****************************************************************     

C     SPECIFICATION PART      

      SUBROUTINE STIFFC                                               
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
      DIMENSION ESTIF(6,6)                                           
C                                                                       
C     EVALUATION OF MEMBER STIFFNESS MATRICES FOR PIN JOINTED          
C     PLANE FRAMEWORKS                                                
C                                                                       
      REWIND 1                                                        
      DO 20 IELEM=1,NELEM                                              
      LPROP=MATNO(IELEM)                                               
      YOUNG=PROPS(LPROP,1)                                             
C	YOUNG=E
C	XAREA=A
C	DINERSI=I
      XAREA=PROPS(LPROP,2)                                             
      DINERSI=PROPS(LPROP,3)
      NODE1=LNODS(IELEM,1)                                              
      NODE2=LNODS(IELEM,2)                                              
      XPROJ=COORD(NODE2,1)-COORD(NODE1,1)                               
      YPROJ=COORD(NODE2,2)-COORD(NODE1,2)                             
      ELENG=SQRT(XPROJ*XPROJ+YPROJ*YPROJ)                              
      SINTH=YPROJ/ELENG                                                
      COSTH=XPROJ/ELENG                                                 

C	SINTH=SIN(X)
C	COSTH=COS(X)

      X=YOUNG*XAREA/ELENG
      Y=YOUNG*DINERSI/(ELENG**3)

C	X=EA/L
C	Y=EI/(L^3)

      ESTIF(1,1)=X*(COSTH)**2+12*Y*(SINTH)**2
      ESTIF(1,2)=X*(COSTH)*(SINTH)-12*Y*(COSTH)*(SINTH)
      ESTIF(1,3)=-6*(YOUNG)*(DINERSI)*(SINTH)/(ELENG)**2
      ESTIF(1,4)=-ESTIF(1,1)
      ESTIF(1,5)=-ESTIF(1,2)
      ESTIF(1,6)=ESTIF(1,3)
      ESTIF(2,1)=ESTIF(1,2)
      ESTIF(2,2)=X*(SINTH)**2+12*Y*(COSTH)**2
      ESTIF(2,3)=6*(YOUNG)*(DINERSI)*(COSTH)/(ELENG)**2
      ESTIF(2,4)=-ESTIF(1,2)
      ESTIF(2,5)=-ESTIF(2,2)
      ESTIF(2,6)=ESTIF(2,3)
      ESTIF(3,1)=ESTIF(1,3)
      ESTIF(3,2)=ESTIF(2,3)
      ESTIF(3,3)=4*(YOUNG)*(DINERSI)/(ELENG)
      ESTIF(3,4)=-ESTIF(1,3)
      ESTIF(3,5)=-ESTIF(2,3)
      ESTIF(3,6)=2*(YOUNG)*(DINERSI)/(ELENG)
      ESTIF(4,1)=-ESTIF(1,1)
      ESTIF(4,2)=-ESTIF(1,2)
      ESTIF(4,3)=-ESTIF(1,3)
      ESTIF(4,4)=-ESTIF(1,4)
      ESTIF(4,5)=-ESTIF(1,5)
      ESTIF(4,6)=-ESTIF(1,6)
      ESTIF(5,1)=-ESTIF(2,1)
      ESTIF(5,2)=-ESTIF(2,2)
      ESTIF(5,3)=-ESTIF(2,3)
      ESTIF(5,4)=-ESTIF(2,4)
      ESTIF(5,5)=-ESTIF(2,5)
      ESTIF(5,6)=-ESTIF(2,6)
      ESTIF(6,1)=ESTIF(1,3)
      ESTIF(6,2)=ESTIF(3,2)
      ESTIF(6,3)=ESTIF(3,6)
      ESTIF(6,4)=-ESTIF(1,3)
      ESTIF(6,5)=-ESTIF(2,3)
      ESTIF(6,6)=ESTIF(3,3)
      DO 100 I=1,6
      DO 100 J=1,6
  100 WRITE(1,*)ESTIF(I,J)                                            
   20 CONTINUE                                                        
      RETURN                                                          
      END                                                              




C     ****************************************************************  
C         THIS THIS ROUTINE ASSEMBLES APPLIED LOADS AND FIXED END SHEAR & MOMENTS
C		TO FORM THE GLOBAL FORCE VECTOR & ASSEMBELS ELEMENTAL STIFFNESS MATRIXS
C		TO FORM STIFFNESS MATRICE.
C		NODAL APPLIED LOADS = RLOAD(I)
C		GLOBAL FORCE VECTOR	= ELOAD
C		LOCAL STIFF. MATRIX	= ESTIF
C		GLOBAL STIFF. MATRIX= ASTIF
C     ****************************************************************  
	SUBROUTINE ASSEMB                                                 
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
     
  
    
      DIMENSION ESTIF(6,6)                                           

C     THIS ROUTINE ASSEMBLES THE ELEMENT (MEMBER) STIFFNESSES AND APPLIED
C	LOADS TO FORM THE GLOBAL STIFFNESS MATRIX AND FORCE VECTOR         
      DO 12 I=1,100
      DO 12 J=1,100    
      NFUNC(I,J)=(J-I)*(2*NSVAB+1-J+i)/2+I
12    CONTINUE
      
      REWIND 1                                                        
      DO 10 ISVAB=1,NSVAB                                             
      ASLOD(ISVAB)=0.0                                                 
      DO 10 JSVAB=1,NSVAB                                             
      ASTIF(ISVAB,JSVAB)=0.0                                          
   10 CONTINUE                                                       
      
C                                                                      
C     ASSEMBLE THE ELEMENT LOADS                                     
C     
      
      DO 15 IPOIN=1,NPOIN                                              
      DO 15 IDOFN=1,NDOFN                                            
      NROWS=(IPOIN-1)*NDOFN+IDOFN                                      
   15 ASLOD(NROWS)=ASLOD(NROWS)+RLOAD(IPOIN,IDOFN)                    
      DO 30 IELEM=1,NELEM                                             
      DO 1 I=1,6
      DO 1 J=1,6
    1 READ(1,*) ESTIF(I,J)                                           
      DO 20 INODE=1,NNODE                                             
      NODEI=LNODS(IELEM,INODE)                                      
      DO 20 IDOFN=1,NDOFN                                             
      NROWS=(NODEI-1)*NDOFN + IDOFN                                  
      NROWE=(INODE-1)*NDOFN + IDOFN                                    

C	THEN ELEMENT SAGGING MOMENT IN EACH DEGREE OF FREEDOM IS ADDED TO
C	GLOBAL LOAD VECTOR (ASLOD) WITH NEGATIVE SIGN

      ASLOD(NROWS)=ASLOD(NROWS) - ELOAD(IELEM,NROWE)                  
C                                                                     
C     ASSEMBLE THE ELEMENT STIFFNESS MATRICES                        
C        
      IF (IFLAG.GT.1) GOTO 63
      DO 20 JNODE = 1,NNODE                                           
      NODEJ=LNODS(IELEM,JNODE)                                        
      DO 20 JDOFN =1,NDOFN                                             
      NCOLS=(NODEJ-1)*NDOFN + JDOFN                                    
      NCOLE=(JNODE-1)*NDOFN + JDOFN 
      ASTIF(NROWS,NCOLS)=ASTIF(NROWS,NCOLS)+ESTIF(NROWE,NCOLE) 
      IF (NCOLS.LT.NROWS) GOTO 20
      NDIFF=NCOLS-NROWS
      IF (NDIFF.GT.NBAND) NBAND=NDIFF
      NGASH=NFUNC(NROWS,NCOLS)

      FSTIF(NGASH)=FSTIF(NGASH)+ESTIF(NROWE,NCOLE)


      
   63 CONTINUE 
   20 CONTINUE                                                        
   30 CONTINUE
     
      NHALF=NBAND+1
      INDEX=NSVAB-NBAND
      NSTIF=NFUNC(INDEX,NSVAB)
      
      IF(NSTIF.LE.MSTIF) RETURN
      
      
      RETURN                                                        
      END                                                          

C     ****************************************************************
C         THIS SUBROUTINE REDUCES THE GLOBAL STIFFNESS EQUATIONS BY 
C         DIRECT GAUSSIAN ELIMINATION
C     ****************************************************************
      SUBROUTINE GREDUC 
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  

      
      DO 12 I=1,100
      DO 12 J=1,100    
      NFUNC(I,J)=((J-I)*(2*NSVAB+1-J+I)/2)+I
12    CONTINUE
      
      NEQNS=NSVAB 
      
      
      K=1
      IF (IFLAG.GT.1)GOTO 51
      DO 50 IEQNS=1,NEQNS
      NLOCA=IEQNS+NBAND
      IF(NLOCA.GT.NEQNS) NLOCA=NEQNS
      IF(IFPRE(IEQNS).EQ.1) GOTO 30 
C 
C     REDUCE EQUATIONS 
C  
      NGASH=NFUNC(IEQNS,IEQNS)

      PIVOT=FSTIF(NGASH)
	IF(ABS(PIVOT)<1.0E-10) GOTO 60    
      IF(IEQNS.EQ.NEQNS) GOTO 50    
      IEQN1=IEQNS+1    
      DO 20 IROWS=IEQN1,NLOCA
      NGASH=NFUNC(IEQNS,IROWS)
      FACTR=FSTIF(NGASH)/PIVOT 
      FACTOR(K)=FACTR
      K=K+1
      IF(FACTR.EQ.0.0) GOTO 20
      DO 10 ICOLS=IEQN1,NLOCA
      IF(IROWS.GT.ICOLS) GOTO 10
      NGESH=NFUNC(IROWS,ICOLS)
      NGISH=NFUNC(IEQNS,ICOLS)
      FSTIF(NGESH)=FSTIF(NGESH)-FACTR*FSTIF(NGISH)    
   10 CONTINUE
      
      ASLOD(IROWS)=ASLOD(IROWS)-FACTR*ASLOD(IEQNS) 
   20 CONTINUE  
      GOTO 50  

      
   30 DO 40 IROWS=IEQNS,NLOCA
      NGASH=NFUNC(IEQNS,IROWS)
      
      ASLOD(IROWS)=ASLOD(IROWS)-FSTIF(NGASH)*FIXED(IEQNS)  
   40 CONTINUE 
      
      NGISH=NFUNC(IEQNS,IEQNS)
      FSTIF(NGISH)=0.0
   50 CONTINUE 

   51 CONTINUE 
C      
C     MAKE GUESS REDUCTION TWO PART
C      

      IF (IFLAG.EQ.1) GOTO 151
      K2=1
      DO 150 IEQNS=1,NEQNS
      NLOCA=IEQNS+NBAND
      IF(NLOCA.GT.NEQNS) NLOCA=NEQNS
      IF(IFPRE(IEQNS).EQ.1) GOTO 130 
      
      IF(IEQNS.EQ.NEQNS) GOTO 150    
      IEQN1=IEQNS+1    
      DO 120 IROWS=IEQN1,NLOCA
      
      ASLOD(IROWS)=ASLOD(IROWS)-FACTOR(K2)*ASLOD(IEQNS) 
      K2=K2+1
  120 CONTINUE
      GOTO 150  
      
  130 DO 140 IROWS=IEQNS,NLOCA
      NGASH=NFUNC(IEQNS,IROWS)
      
      ASLOD(IROWS)=ASLOD(IROWS)-FSTIF(NGASH)*FIXED(IEQNS)  
  140 CONTINUE 
      

  150 CONTINUE 
  151 CONTINUE     
      
      
      
      
      
      IFLAG=2
      RETURN     
   60 CONTINUE
      STOP
      END                                                       
C     ****************************************************************
C         THIS SUBROUTINE SOLVES THE LOWER TRINGULAR SET OF LINEAR 
C	    EQUATIONS BY USE OF BACK SOLVE SYSTEM.
C     ****************************************************************
      SUBROUTINE BAKSUB                                                 
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
     
C                                                                      
C     THIS ROUTINE PERFORMS THE BACK-                                 
C     SUBSTITUTION PHASE                                               
C                                                                     
      DO 12 I=1,100
      DO 12 J=1,100    
      NFUNC(I,J)=((J-I)*(2*NSVAB+1-J+I)/2)+I
12    CONTINUE
      DO 123 J=1,80    
      XDISP(J)=0
123   CONTINUE
      
      NEQNS=NSVAB                                                      
      DO 5 IEQNS=1,NEQNS                                             
      REACT(IEQNS)=0.0                                                
    5 CONTINUE                                                        
      NEQN1=NEQNS+1                                                   
      DO 30 IEQNS=1,NEQNS                                            
      NBACK=NEQN1-IEQNS                                               
      NGASH=NFUNC(NBACK,NBACK)
      PIVOT=FSTIF(NGASH)    
      RESID=ASLOD(NBACK)
           
      
      IF(NBACK.EQ.NEQNS) GOTO 20 
      NBAC1=NBACK+1     
      NLOCA=NBACK+NBAND
      
      DO 10 ICOLS=NBAC1,NLOCA
      NGESH=NFUNC(NBACK,ICOLS)
      RESID=RESID-FSTIF(NGESH)*XDISP(ICOLS)  
   10 CONTINUE
  20  IF(IFPRE(NBACK).EQ.0) XDISP(NBACK)=RESID/PIVOT      
      IF(IFPRE(NBACK).EQ.1) XDISP(NBACK)=FIXED(NBACK)     
      IF(IFPRE(NBACK).EQ.1) REACT(NBACK)=-RESID 
      
   30 CONTINUE
      KOUNT=0  
      
      DO 40 IPOIN=1,NPOIN
      DO 40 IDOFN=1,NDOFN     
      KOUNT=KOUNT+1
      TDISP(IPOIN,IDOFN)=XDISP(KOUNT)
   40	TREAC(IPOIN,IDOFN)=REACT(KOUNT)
      RETURN                                                            
      END                                                               
C     ****************************************************************  
C        THIS SUBROUTINE COMPUTES 3 NODAL INTERNAL FORCES.
C     ****************************************************************  
     
      SUBROUTINE FORCE                                                
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ESTIF(6,6),FOMEM(6)                                  
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
                      
C                                                                     
C     MEMBER FORCE CALCULATIONS FOR BOTH AXIAL                        
C     BAR AND PIN-JOINTED PLANE FRAME PROBLEMS                        
C                                                                    
      REWIND 1                                                       
      DO 10 IELEM=1,NELEM                                            
      READ(1,*) ESTIF                                             
C                                                                       
C     EVALUATE THE MEMBER END FORCES                                   
C                                                                     
      DO 20 IEVAB=1,NEVAB                                              
      FOMEM(IEVAB)=0.0                                                  
      KOUNT=0                                                           
      DO 20 INODE=1,NNODE                                               
      LOCAL=LNODS(IELEM,INODE)                                          
      DO 20 IDOFN=1,NDOFN                                               
      KOUNT=KOUNT+1                                                     
   20 FOMEM(IEVAB)=FOMEM(IEVAB)+ESTIF(IEVAB,KOUNT)*TDISP(LOCAL,IDOFN)   
      FOMEM(1)=FOMEM(1)+ELOAD(IELEM,1)
      FOMEM(2)=FOMEM(2)+ELOAD(IELEM,2)
      FOMEM(3)=FOMEM(3)+ELOAD(IELEM,3)
      FOMEM(4)=FOMEM(4)+ELOAD(IELEM,4)
      FOMEM(5)=FOMEM(5)+ELOAD(IELEM,5)
      FOMEM(6)=FOMEM(6)+ELOAD(IELEM,6)
C                                                                    
C     EVALUATE THE AXIAL FORCE                                         
C                                                                     
      LPROP=MATNO(IELEM)
      YOUNG=PROPS(LPROP,1)
      XAREA=PROPS(LPROP,2)
      DINERSI=PROPS(LPROP,3)
      NODE1=LNODS(IELEM,1)
      NODE2=LNODS(IELEM,2)
      XPROJ=COORD(NODE2,1)-COORD(NODE1,1)
      YPROJ=COORD(NODE2,2)-COORD(NODE1,2)
      ELENG=SQRT(XPROJ*XPROJ+YPROJ*YPROJ)
      SINTH=YPROJ/ELENG
      COSTH=XPROJ/ELENG
      STRES(IELEM,1)=FOMEM(1)*COSTH+FOMEM(2)*SINTH
      STRES(IELEM,2)=FOMEM(2)*COSTH-FOMEM(1)*SINTH
      STRES(IELEM,3)=FOMEM(3)
      STRES(IELEM,4)=FOMEM(4)*COSTH+FOMEM(5)*SINTH
      STRES(IELEM,5)=FOMEM(5)*COSTH-FOMEM(4)*SINTH
   10 STRES(IELEM,6)=FOMEM(6)
      RETURN                                                          
      END  


C     ****************************************************************  
C                   THIS SUBROUTINE OUTPUTS THE RESULTS
C     ****************************************************************  

      SUBROUTINE RESULT                                                
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
                  
C                                                                      
C     WRITE THE NODAL(JOINT) DISPLACEMENTS AND REACTIONS                
C                                                                      
      WRITE(6,900)                                                      
  900 FORMAT(/,1H0,5X,4HNODE,1X,30HDISPLACEMENTS (DX,DY,ROTATION))     
      DO 10 IPOIN=1,NPOIN                                              
   10 WRITE(6,910) IPOIN,(TDISP(IPOIN,IDOFN),IDOFN=1,NDOFN)            
  910 FORMAT(5X,1I3,3F15.5)                                            
      WRITE(6,991)
  991 FORMAT(/,1H0,5X,4HNODE,1X,19HREACTIONS (RX,RY,M))
      DO 21 IPOIN=1,NPOIN
   21 WRITE(6,990)IPOIN,(TREAC(IPOIN,IDOFN),IDOFN=1,NDOFN)
  990 FORMAT(5X,1I3,3F15.5)
C                                                                     
C     WRITE THE ELEMENT(MEMBER) STRESSES(FORCES)                       
C                                                                      
      IF(NSTRE.EQ.0) GO TO 30                                          
      WRITE(6,920)                                                      
  920 FORMAT(/,1H0,5X,7HELEMENT,6X,29HNODAL INTERNAL FORCES (N,V,M))    
      DO 20 IELEM=1,NELEM                                              
      WRITE(6,922) IELEM,(STRES(IELEM,ISTRE),ISTRE=1,3)                
   20 WRITE(6,923) (STRES(IELEM,ISTRE),ISTRE=4,6)
  922 FORMAT(5X,I3,6X,3F15.5)
  923 FORMAT(14X,3F15.5,/)
   30 CONTINUE
      
      
       DO 25 IELEM=1,NELEM
      T=STRES(IELEM,3)
      
      DO 26 JELEM=IELEM,2,-1
      STRES(JELEM,3)=STRES(JELEM-1,3)
      
      IF (T.LT.STRES(JELEM,3)) GOTO 22
      
   26 CONTINUE
   22 CONTINUE     

      
      STRES(JELEM,3)=T
   25 CONTINUE  
      
      WRITE(6,*) "MAXIMUM OF FIRST MOMENTS="
      WRITE(6,*) STRES(1,3)
      
      DO 35 IELEM=1,NELEM
      T=STRES(IELEM,6)
      
      DO 36 JELEM=IELEM,2,-1
      STRES(JELEM,6)=STRES(JELEM-1,6)
      
      IF (T.LT.STRES(JELEM,6)) GOTO 32
      
   36 CONTINUE
   32 CONTINUE     

      
      STRES(JELEM,6)=T
   35 CONTINUE 
      
       WRITE(6,*) "MAXIMUM OF END MOMENTS=" 
       WRITE(6,*) STRES(1,6)
      
      RETURN                                                           
      END                                                              
      
      SUBROUTINE SORT                                                
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/FEM1/NPOIN,NELEM,NBOUN,NPROP,NNODE,NEVAB,NSVAB,NDOFN,
     -	 NDIME,NSTRE,NMATS,NBAND,NCASE,NLOCA,IFLAG
      COMMON/FEM2/PROPS(5,3),COORD(40,2),LNODS(75,2),IFPRE(80),        
     .            FIXED(80),RLOAD(40,3),ELOAD(75,6),MATNO(75),          
     .            STRES(75,6),XDISP(80),TDISP(40,3),TREAC(40,3),        
     .            ASTIF(80,80),ASLOD(80),REACT(80),FSTIF(1600)
     .            ,NFUNC(100,100),FACTOR(300)  
      
       DO 25 IELEM=1,NELEM
      T=STRES(IELEM,3)
      
      DO 26 JELEM=IELEM,2,-1
      STRES(JELEM,3)=STRES(JELEM-1,3)
      
      IF (T.LT.STRES(JELEM,3)) GOTO 22
      
   26 CONTINUE
   22 CONTINUE     

      
      STRES(JELEM,3)=T
   25 CONTINUE  
      
      WRITE(6,*) "MAXIMUM OF FIRST MOMENTS="
      WRITE(6,*) STRES(1,3)
      
      DO 35 IELEM=1,NELEM
      T=STRES(IELEM,6)
      
      DO 36 JELEM=IELEM,2,-1
      STRES(JELEM,6)=STRES(JELEM-1,6)
      
      IF (T.LT.STRES(JELEM,6)) GOTO 32
      
   36 CONTINUE
   32 CONTINUE     

      
      STRES(JELEM,6)=T
   35 CONTINUE 
      
       WRITE(6,*) "MAXIMUM OF END MOMENTS=" 
       WRITE(6,*) STRES(1,6)
      
      RETURN                                                           
      END                  
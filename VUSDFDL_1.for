      subroutine vusdfld(
     1   nblock, nstatev, nfieldv, nprops, ndir, nshr, 
     2   jElem, kIntPt, kLayer, kSecPt, 
     3   stepTime, totalTime, dt, cmname, 
     4   coordMp, direct, T, charLength, props, 
     5   stateOld, 
     6   stateNew, field )

c
      include 'vaba_param.inc'
c

      dimension jElem(nblock), coordMp(nblock,*), 
     1          direct(nblock,3,3), T(nblock,3,3), 
     2          charLength(nblock), props(nprops), 
     3          stateOld(nblock,nstatev), 
     4          stateNew(nblock,nstatev),
     5          field(nblock,nfieldv)
      character*80 cmname
      ! SDV (stateNew,stateOld ) vs field
      ! SDV are used for iterative process, which means, values from previous iteration
      ! field = variables are computed based on SDV and others. cannot access previous values
      ! in this subroutine, SDV = 2(sigma_eq_f/m),2(delta_eq_f/m), 3(damage_f/m/s)
      ! field = 3(stress_11/22/12), 3(damage_f/m/s), 2(force_11/22) 

c
c     Local arrays from vgetvrm are dimensioned to 
c     maximum block size (maxblk)
c
      parameter( nrData=6 )
      character*3 cData(maxblk*nrData)
      dimension rData(maxblk*nrData), jData(maxblk*nrData)
c
c     beta = hyperparameter, always less than 1 and greater than 0.
c 
      parameter (beta = 0.8, alpha = 0.0)

c      
c     CALL UTILITY SUBROTINE TO GET CURRENT area of element,STRESS AND STRAIN
c      
      jStatus = 1      
      ! for stress at current block of integration point
      call vgetvrm( 'S', stress, rData, jData, cData, jStatus ) 
      if( jStatus .ne. 0 ) then
         call xplb_abqerr(-2,'satish_Utility routine VGETVRM '//
     *      'failed to get variable.',0,zero,' ')
         call xplb_exit
      end if
      ! for strain at current block of integration point
      call vgetvrm( 'E', strain, rData, jData, cData, jStatus ) 
      if( jStatus .ne. 0 ) then
         call xplb_abqerr(-2,'satish_Utility routine VGETVRM '//
     *      'failed to get variable.',0,zero,' ')
         call xplb_exit
      end if

c     
c     A NEW SUBROTINE TO PERFORM ALL CALCUALTIONS
c
      ! Modified Hashin matrix compression model (MHMCM)
      call MHMCM(nstatev,nblock,nprops,charLength,
     1 props,stress,strain,stateNew,stateOld,alpha,beta)
      
      return
      end

     
      subroutine MHMCM(nstatev,nblock,nprops,charLength,
     1 props,stress,strain,stateNew,stateOld,alpha,beta)
c
      include 'vaba_param.inc'
c
      dimension charLength(nblock), props(nprops), 
     1          stateOld(nblock,nstatev), 
     2          stateNew(nblock,nstatev)
 
c
c     define all variables
c
      
        do k = 1, nblock
c
c     define peinciple stress and strain 
c      

         
c
c     charateristic length for given integration point
c

        
c
c     inititalize SDV's by stateNew=stateOld
c


c
c     Fiber failure
c


c
c     Matrix failure
c

c      
c     compute force at the integration point using SF2=sigma22*area
c

      end do   


      return
      end 
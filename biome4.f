c---------------------------------------------------------------------------
c    The BIOME4-system:       biome4.f       4.2b2       02.11.99
c
c       Copyright (c) 1999 by Jed O. Kaplan
c     
c       See COPYING file for copying and redistribution conditions.
c
c       This program is free software; you can redistribute it and/or modify
c       it under the terms of the GNU General Public License as published by
c       the Free Software Foundation; version 2 of the License.
c
c       This program is distributed in the hope that it will be useful,
c       but WITHOUT ANY WARRANTY; without even the implied warranty of
c       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c       GNU General Public License for more details.
c
c       Contact info: jkaplan@bgc-jena.mpg.de
c---------------------------------------------------------------------------
c
c                            B I O M E 4 . F
c
c---------------------------------------------------------------------------
c      This is BIOME4, based on BIOME3 (Haxeltine and Prentice 1996).
c
c      All arguments to and from this subroutine are passed through two arrays: 
c
c              "vars_in" and "output"
c
c      You can customize the contents of these arrays to suit your own needs
c      based on the assignments in the beginning of the program and the 
c      output assignments in the subroutine "competition2".  I suggest each
c      user write his own driver software.
c
c      The basic input variables required by this model are:
c      Monthly mean- temperature, precipitation, %cloudiness (or sunshine)
c      Absolute minimum temperature
c      Water holding capacity (mm) in the top 30cm of the soil
c      Water holding capacity (mm) in the rest of the soil
c      Conductivity index of water through the soil column (see the soil
c      and hydrology subroutines for more information).
c
c      Caveat: compared to the original BIOME3 there is a lot of added
c      functionality in this model, and in some cases complexity.  The code
c      may neither compile nor run on certain systems (it works well now
c      compiled with g77 and running on an Intel Linux machine).
c
c      Please direct your questions to me at jed.kaplan@bgc-jena.mpg.de
c
c      By Jed O. Kaplan 1994-1999.
c
c      For further reference see:
c
c       Haxeltine and Prentice, I.C. 1996. BIOME3: An equilibrium 
c       terrestrial biosphere model based on ecophysiological 
c       constraints, resource availibility and competition among
c         plant functional types, Global Biogeochemical Cycles, 10(4) 693-709.
c
c      Please remove this header and change the name of your new model
c      if you make ANY alterations to the code!
c
c      Author:       Jed O. Kaplan
c      Date:       22 October 1999
c      Version:       v4.2b2
c      Revised: 02.11.99 by JOK
c       Added back in d13C/DeltaE functionality
c      Revised: 18.11.99 by JOK
c       Changed competition subroutine (line 708-709),
c       so that npp=npp(dom), not npp=npp(wdom).
c      Revised: 15.12.99 by JOK
c       Added functionality to tune soil parameters when the model
c       is running in diagnostic mode. (line 240-251)
c       Modified hydrology subroutine to handle low moisture retention
c       soils (line 2356-2364) with else statements setting w to 0 when
c       k=0 (this should be true)
c      Revised: 17.12.99 by JOK
c       Changed this header information to reflect what the soils data
c       actually is.
c       Examined hydrology subroutine to establish sensitivity to soil
c       physical properties.
c       Corrected line 2348 percolation function should always be to the 
c       power 4 (**4) and not scaled to k().
c
c------------------------------------------------------------------------

      subroutine biome4(vars_in,output)

      implicit none

      integer numofpfts

      parameter(numofpfts=13)

      logical diagmode

      integer optdata(0:numofpfts,500)
      integer biome,pft
      integer pfts(numofpfts),iopt
      integer i

      real lon,lat,co2,p,tdif,temp(12),prec(12),clou(12),soil(5)
      real dtemp(365),dprec(365),dclou(365),dprecin(365)
      real dpet(365),dphen(365,2),dmelt(365),realout(0:numofpfts,200)
      real optnpp(0:numofpfts),optlai(0:numofpfts)
      real pftpar(25,25),k(12),dayl(12),sun(12),wetness(0:numofpfts)
      real gdd5,tcm,twm,tprec,gdd0,rad0,tmin,tminin
      real tsoil(12),ddayl(365),maxdepth
      real radanom(12),alttmin

      real vars_in(50)
      real output(500)
      
      character*1 yorn
      
      real sumagnpp,delag,wtagnpp
      
c----------------------------

      character*40 biomename(28)
      
      data biomename /
     >      'Tropical evergreen forest',
     >      'Tropical semi-deciduous forest',
     >      'Tropical deciduous forest/woodland',
     >      'Temperate deciduous forest',
     >      'Temperate conifer forest',
     >      'Warm mixed forest',
     >      'Cool mixed forest',
     >      'Cool conifer forest',
     >      'Cold mixed forest',
     >      'Evegreen taiga/montane forest',
     >      'Deciduous taiga/montane forest',
     >      'Tropical savanna',
     >      'Tropical xerophytic shrubland',
     >      'Temperate xerophytic shrubland',
     >      'Temperate sclerophyll woodland',
     >      'Temperate broadleaved savanna',
     >      'Open conifer woodland',
     >      'Boreal parkland',
     >      'Tropical grassland',
     >      'Temperate grassland',
     >      'Desert',
     >      'Steppe tundra',
     >      'Shrub tundra',
     >      'Dwarf shrub tundra',
     >      'Prostrate shrub tundra',
     >      'Cushion-forbs, lichen and moss',
     >      'Barren',
     >      'Land ice' /

c----------------------------
c     assign the variables that arrived in the array vars_in

      lon=vars_in(49)
      lat=vars_in(1) !vars_in(49)-(vars_in(1)/vars_in(50))
      co2=vars_in(2)
      p=vars_in(3)
      tminin=vars_in(4)
      
      do i=1,12
       temp(i)=vars_in(4+i)
       prec(i)=vars_in(16+i)
       clou(i)=vars_in(28+i)
      end do
       
      do i=1,4
       soil(i)=vars_in(40+i)
      end do
      
      iopt=nint(vars_in(46))

      if (iopt.eq.1.) then
       diagmode=.true.
      else
       diagmode=.false.
      end if
      
c----------------------------
 
c      set a dummy rad anomaly (not used in this version)
       do i=1,12
        radanom(i)=1.0
       end do

c-------------------------------------------------------------------------
c      Reset the output matrix
       do pft=0,numofpfts
        do i=1,500
         optdata(pft,i)=0
        end do
       end do
c-------------------------------------------------------------------------
c      Initialize soil texture specific parameters
       k(1)=soil(1)
       k(2)=soil(2)
       k(5)=soil(3)
       k(6)=soil(4)
c-------------------------------------------------------------------------
c      Linearly interpolate mid-month values to quasi-daily values:
       call daily(temp,dtemp)
       call daily(clou,dclou)   
       call daily(prec,dprecin)   
c--------------------------------------------------------------------------
c      Initialize parameters derived from climate data:
       call climdata(tcm,twm,gdd5,gdd0,tprec,temp,prec,dtemp,alttmin)
           
       call soiltemp(temp,soil,tsoil)
c--------------------------------------------------------------------------
c      Calculate mid-month values for pet,sun & dayl from temp,cloud & lat:
       call ppeett
     >  (lat,dtemp,dclou,dpet,temp,sun,dayl,rad0,ddayl,radanom)  
c-------------------------------------------------------------------------
c      Run snow model:
       call snow(dtemp,dprec,dmelt,dprecin,maxdepth)   
c-------------------------------------------------------------------------

c      Initialize the evergreen phenology
       do i=1,365
        dphen(i,1)=1.0
        dphen(i,2)=1.0
       end do

c      Initialize pft specific parameters
       call pftdata(pftpar)   

c--------------------------------------------------------------------------
c      Rulebase of absolute constraints to select potentially presents pfts:  

        call constraints
     >      (tcm,twm,tminin,gdd5,rad0,pfts,tmin,maxdepth,gdd0)

c       The tropical evergreen pft is not used in this version
c       of the model.  This is because the tropical deciduous tree
c       will be evergreen if it is not subject to water stress.
c       Otherwise the two pft's are parameterized in the same way,
c       so not using the pft saves computation time.

        pfts(1)=0

c--------------------------------------------------------------------------

      if (diagmode) then

       write(*,'(A,F8.2,A,F8.2)')'Longitude:',lon,' Latitude:',lat
       write(*,'(A,F6.1,A,F6.1,A)')
     >    'Tcm and Tmin are:',tcm,' and',tmin,' degrees C respectively.'
       write(*,'(A,F8.1,A,F8.1,A)')
     >    'GDD5 is:',gdd5,' and total annual precip is:',tprec,' mm.'
       write(*,'(A,F8.1,A)')
     >    'Maximum snowdepth is:',maxdepth*10.,' mm.'
       write(*,'(A,2F7.2,3F7.1)')
     >    'The current soil parameters are:',soil
       write(*,'(A,$)')
     >    'Enter new soil parameters? '
       read(*,*)yorn
       if (yorn.eq.'y') then
        write(*,'(A,$)')'Percolation index ~(0-7): '
        read(*,*)soil(1)
        soil(2)=soil(1)
        write(*,'(A,$)')'Top layer whc ~(0-999): '
        read(*,*)soil(3)
        write(*,'(A,$)')'Bottom layer whc ~(0-999): '
        read(*,*)soil(4)

c       Reinitialize soil texture specific parameters
        k(1)=soil(1)
        k(2)=soil(2)
        k(5)=soil(3)
        k(6)=soil(4)

       end if
       
       write(*,*)'The following PFTs will be computed:'

      end if

c--------------------------------------------------------------------------
c     Calculate optimal LAI & NPP for the selected pfts:
        
      do pft=1,numofpfts

       optlai(pft)=0.0
       optnpp(pft)=0.0

       if (pfts(pft).ne.0) then

        if (pftpar(pft,1).ge.2) then
c        Initialize the generic summergreen phenology
         call phenology(dphen,dtemp,temp,tcm,tdif,tmin,pft,ddayl,pftpar)
        end if

        call findnpp(pfts,pft,optlai(pft),optnpp(pft),
     >  tprec,dtemp,sun,temp,dprec,dmelt,dpet,dayl,k,pftpar,optdata,
     >  dphen,co2,p,tsoil,realout,numofpfts)

       end if
      end do
c------------------------------------------------------------------------------
c      Select dominant plant type/s on the basis of modelled optimal NPP & LAI:
       
       call competition2
     > (optnpp,optlai,wetness,tmin,tprec,pfts,optdata,output,diagmode,
     >  biome,numofpfts,gdd0,gdd5,tcm,pftpar,soil)

c------------------------------------------------------------------------------
c      Final output biome is given by the integer biome:
 
       output(1)=biome
       output(48)=lon
       output(49)=lat
       

       do pft=1,numofpfts
       
        output(300+pft)=nint(optnpp(pft))
        output(300+numofpfts+pft)=nint(optlai(pft)*100.0)
       
c-----------------------
        if (pfts(pft).ne.0) then
        if (diagmode) then
c       type some diagnostic output here
         write(*,10)pft,optlai(pft),optnpp(pft),wetness(pft),
     >    ((optdata(pft,i)/10.),i=37,48) 
10       format(I3,F5.2,F7.1,F6.1,12F6.1)
        end if
        end if

c-----------------------

       end do
       
      if (diagmode) then
       write(*,'(A,I3,A,A)')'Biome',biome,' ',biomename(biome)
       
       sumagnpp=0.0
       delag=0.0
       do i=1,6
        if (optdata(8,36+i).gt.0) then
         sumagnpp=sumagnpp+(real(optdata(8,36+i))/10.)
       end if
       end do

       do i=1,6
        if (optdata(8,36+i).gt.0) then
       wtagnpp=real(optdata(8,36+i))/(sumagnpp*10.)
        delag=delag+real((optdata(8,79+i))*wtagnpp/100.)
        end if
       end do
       
       write(*,'(A,F6.2,A)')
     > 'The deltaA of C3 grass is',delag,' per mil.'

       write(*,*)'press return to continue'
       read(*,*)
      end if
     
      return
      end

c*******************************************************************************
      subroutine competition2
     >(optnpp,optlai,wetness,tmin,tprec,pfts,optdata,output,diagmode,
     > biome,numofpfts,gdd0,gdd5,tcm,pftpar,soil)

      implicit none

      integer numofpfts

      real optnpp(0:numofpfts),optlai(0:numofpfts),maxnpp,maxlai
      real maxdiffnpp,woodylai,grasslai,driest(0:14)  !numofpfts
      real tmin,tcm,pftpar(25,25),soil(5),mwet,wettest(0:14)
      real temperatenpp,tprec,wetness(0:numofpfts),lai,npp
      real woodnpp,grassnpp,subnpp,gdd0,gdd5,ratio,lairatio,nppdif
      real wetlayer(0:numofpfts,2),wetratio(0:numofpfts)
      
      real npprat,treepct,grasspct
      
      real output(500)

c      real woodypercent,grasspercent

      integer pft,pftmaxlai,pftmaxnpp,optpft,subpft
      integer pfts(numofpfts),optdata(0:numofpfts,500)
      integer dom,wdom,grasspft,m,pos,month,wetpft,firedays
      integer biome,greendays,drymonth(0:14),subfiredays !numofpfts

      logical grass(14),present(14),flop  !numofpfts
      logical diagmode

      do pft=1,numofpfts
       if (pft.ge.8) then
        grass(pft)=.true.
       else
        grass(pft)=.false.
       end if

       grass(10)=.false.

       if (optnpp(pft).gt.0.0) then
        present(pft)=.true.
       else
        present(pft)=.false.
       end if
      end do
      
      present(12)=.true.

c----Initialize all of the variables that index an array---
      optpft=0
      subpft=0
      grasspft=0
      pftmaxnpp=0
      pftmaxlai=0
      dom=0
      wdom=0
      wetpft=0

      maxnpp=0.0
      maxlai=0.0
      temperatenpp=0.0
      maxdiffnpp=0.0
      grassnpp=0.0

c------------------------------------------------------------------------

c---- choose the dominant woody PFT on the basis of NPP:

c------------------------------------------------------------------------------
c-----Find the PFTs with the highest npp and lai-------------------------

      do pft=1,12

       if (grass(pft)) then                 !grass PFT's
        if (optnpp(pft).gt.grassnpp) then
         grassnpp=optnpp(pft) 
         grasspft=pft
        end if

       else

        if (optnpp(pft).gt.maxnpp) then     !woody PFT's
         maxnpp=optnpp(pft)
         pftmaxnpp=pft
        end if
        if (optlai(pft).gt.maxlai) then
         maxlai=optlai(pft)
         pftmaxlai=pft
        else if (optlai(pft).eq.maxlai) then
         maxlai=optlai(pftmaxnpp)
         pftmaxlai=pftmaxnpp
        end if
       end if

      end do
 
c-----Find average annual soil moisture value for all PFTs:----------


      do pft=1,numofpfts

       wetness(pft)=0.
       wetlayer(pft,1)=0.0
       wetlayer(pft,2)=0.0
       drymonth(pft)=0
       wettest(pft)=-1.0
       driest(pft)=101.0

       do m=1,12
c        mwet=optdata(pft,m+12)
        mwet=optdata(pft,m+412)
        wetness(pft)=wetness(pft)+mwet/12.
        wetlayer(pft,1)=wetlayer(pft,1)+optdata(pft,m+412)/12.  !top
        wetlayer(pft,2)=wetlayer(pft,2)+optdata(pft,m+424)/12.  !bottom
        if (mwet.gt.wettest(pft)) wettest(pft)=mwet
       if (mwet.lt.driest(pft)) then 
        drymonth(pft)=m
        driest(pft)=mwet
       end if
       end do

      end do
c----------------------------------------

      optpft=pftmaxnpp
      wdom=optpft

c     find the subdominant woody pft (2nd in NPP)

      subnpp=0.0
      subpft=0

      do pft=1,7
       if (pft.ne.wdom) then
        if (optnpp(pft).gt.subnpp) then
         subnpp=optnpp(pft)
         subpft=pft
        end if
       end if
      end do

c-----------------------------------------------------------

      flop=.false.

1     continue

      woodylai= optlai(wdom)
      woodnpp = optnpp(wdom)
      grasslai= optlai(grasspft)

      if (wdom.ne.0) then
       firedays=optdata(wdom,199)
       subfiredays=optdata(subpft,199)
       greendays=optdata(wdom,200)
      else 
       firedays=0
       subfiredays=0
       greendays=0
      end if

      nppdif=optnpp(wdom)-optnpp(grasspft)

      ratio=0.0

c------------------------------------------------------------

      if ((wdom.eq.3.or.wdom.eq.5).and.tmin.gt.0.0) then
       if (gdd5.gt.5000.0) then
        wdom=2
        goto 1
       end if       
      end if

c------------------------------------------------------------
c     Under certain conditions grass will be the dominant or co-dominant PFT:

      if (wdom.eq.1) then
       if (optnpp(wdom).lt.2000.0) then
        wdom=2
       subpft=1
       goto 1
       end if
      end if         

      if (wdom.eq.2) then
       if (woodylai.lt.2.0) then
        optpft=grasspft
       else if (grasspft.eq.9.and.woodylai.lt.3.6) then
        optpft=14
       else if 
     >  (greendays.lt.270.and.tcm.gt.21.0.and.tprec.lt.1700.0) 
     > then
        optpft=14
       else
        optpft=wdom
       end if
      end if

      if (wdom.eq.3) then
       if (optnpp(wdom).lt.140.0) then
        optpft=grasspft
       else if (woodylai.lt.1.0) then
        optpft=grasspft
       else if (woodylai.lt.2.0) then
        optpft=14
       else
        optpft=wdom
       end if
      end if 

      if (wdom.eq.4) then
       if (woodylai.lt.2.0) then
        optpft=grasspft
       else if (firedays.gt.210.and.nppdif.lt.0.0) then
       if (.not.flop.and.subpft.ne.0) then
        wdom=subpft
        subpft=4
        flop=.true.
        goto 1
        else
        optpft=grasspft
       end if
       else if (woodylai.lt.3.0.or.firedays.gt.180) then
        if (nppdif.lt.0.0) then
         optpft=14
       else if (.not.flop.and.subpft.ne.0) then
        wdom=subpft
        subpft=4
        flop=.true.
        goto 1
       end if
       else
       optpft=wdom
       end if
      end if

      if (wdom.eq.5) then
       if (present(3)) then
        wdom=3
       subpft=5
       goto 1
c       else if (nppdif.lt.0.0) then
c       if (.not.flop.and.subpft.ne.0) then
c        wdom=subpft
c        subpft=5
c        flop=.true.
c        goto 1
c       end if  
       else if (optnpp(wdom).lt.140.0) then
        optpft=grasspft
       else if (woodylai.lt.1.2) then
        optpft=14
       else
        optpft=wdom
       end if
      end if 


c     add npp limits on other PFT's (tropical mountain story)

      if (wdom.eq.6) then
       if (optnpp(wdom).lt.140.0) then
        optpft=grasspft
       else if (firedays.gt.90) then
       if (.not.flop.and.subpft.ne.0) then
        wdom=subpft
        subpft=6
        flop=.true.
        goto 1
       end if  
       else
        optpft=wdom
       end if
      end if

      if (wdom.eq.7) then
       if (optnpp(wdom).lt.120.0) then
        optpft=grasspft
c       else if (optnpp(wdom).lt.120.0) then
c        optpft=14
       else if (wetness(wdom).lt.30.0.and.nppdif.lt.0.0) then
        optpft=grasspft
       else       
        optpft=wdom
       end if
      end if                 

      if (wdom.eq.0) then 
       if (grasspft.ne.0) then
        optpft=grasspft
       else if (optnpp(13).ne.0.0) then
        optpft=13
       else
        optpft=0
       end if
      end if

      if (optpft.eq.0.and.present(10)) optpft=10

      if (optpft.eq.10) then
       if (grasspft.ne.9.and.(optnpp(grasspft).gt.optnpp(10))) then
        optpft=grasspft
       else
        optpft=10
       end if
      end if        

      if (optpft.eq.grasspft) then
       if (optlai(grasspft).lt.1.8.and.present(10)) then
        optpft=10
       else
        optpft=grasspft
       end if
      end if

      if (optpft.eq.11) then
       if (wetness(optpft).le.25.0.and.(present(12))) then
        optpft=12
       end if
      end if

c----------------------------------------------------------------------

c----------------------------------------------------------------------

c     output some diagnostic results

      if (diagmode) then
       do pft=1,numofpfts
        if (pfts(pft).ne.0) then
         write(*,4)
     >   pft,drymonth(pft),driest(pft),wetness(pft),optdata(pft,199),
     >   optdata(pft,200)
4        format(2I5,2F6.2,2I5)
        end if
       end do

       write(*,*)' wpft  woodynpp   woodylai gpft grassnpp subpft phi'
       write(*,5)wdom,woodnpp,woodylai,grasspft,grassnpp,subpft,
     >           optdata(8,52)/100.
5      format(I5,F10.2,F10.2,I5,F10.2,I5,F8.2,F8.2)
      
      end if
c------------------------------------------------------

c     put some variables into format for output

      dom=optpft

c------------------------------------------------------
      if (optpft.eq.14) then
      
       npprat=woodnpp/grassnpp

       treepct=((8./5.)*npprat)-.54
       
       if (treepct.lt.0.0) treepct=0.0
       if (treepct.gt.1.0) treepct=1.0
      
       grasspct=1.-treepct

       dom=wdom
       lai=(woodylai+(2.*grasslai))/3.0
       npp=(woodnpp+(2.*grassnpp))/3.0

c      NEP
       do pos=137,148
        optdata(dom,pos)=
     >    (optdata(wdom,pos)+(2.*(optdata(grasspft,pos))))/3.0
       end do

c      DeltaA
       do pos=80,91
        optdata(dom,pos)=
     >    (optdata(wdom,pos)+(2.*(optdata(grasspft,pos))))/3.0
       end do

c      NPP
       do pos=37,48
        optdata(dom,pos)=
     >    (optdata(wdom,pos)+(2.*(optdata(grasspft,pos))))/3.0
       end do

c      Rh
       do pos=113,124
        optdata(dom,pos)=
     >    (optdata(wdom,pos)+(2.*(optdata(grasspft,pos))))/3.0
       end do

c      DeltaE
       optdata(dom,50)=
     > treepct*optdata(wdom,50)+grasspct*optdata(grasspft,50)
c     >    (optdata(wdom,50)+(2.*(optdata(grasspft,50))))/3.0

c      %C4 NPP
       optdata(dom,98)=
     >    (optdata(wdom,98)+(2.*(optdata(grasspft,98))))/3.0
       
      end if
c------------------------------------------------------

      if (optlai(dom).eq.0.0) optpft=0

      npp=optnpp(dom)
      lai=optlai(dom)

c      npp=optnpp(wdom)
c      lai=optlai(wdom)
      grasslai=optlai(grasspft)

c      npp=optnpp(grasspft)
c      lai=optlai(grasspft)

c      lai=woodylai
c      lai=grasslai

      call newassignbiome
     >(optpft,wdom,grasspft,subpft,npp,woodnpp,grassnpp,subnpp,
     > greendays,biome,gdd0,gdd5,tcm,present,woodylai,grasslai,tmin)

c-----------------------------------------------------------------------
c       The values of all output variables, except the actual biome type
c       [output(1)] are assigned here:
 

        output(2)=nint(lai*100.)
        output(3)=nint(npp)  
        output(4)=nint(optlai(wdom)*100.)
        output(5)=nint(optnpp(wdom))
        output(6)=nint(optlai(grasspft)*100.)
        output(7)=nint(optnpp(grasspft))

c       Annual APAR / annual PAR expressed as a percentage:
        output(8) = optdata(dom,8)

c       Respiration costs (for dom plant type, wood or grass):
        output(9) = optdata(dom,9)

c       Soil moisture for dominant pft:
        output(10)= nint(wetness(dom)*10.)

c       Soil moisture for dominant pft:
c        output(10)= nint(wetness(wdom)*10.)

c       Predicted runoff (for dom plant type, wood or grass):
        output(11)= optdata(wdom,6)

c       Number of the dominant (woody) pft:  
        output(12)=optpft
c        output(12)=wdom

c       Total annual precipitation (hopefully<9999mm):
        output(13)=nint(tprec)
        if(output(13).gt.9999) output(13)=9999

c       Total annual PAR MJ.m-2.yr-1
        output(14)=optdata(dom,7)

        output(15)=nint(100*lairatio)

        output(16)=nint(nppdif)

        if (lai.lt.2.0) then               !FVC
         output(17)=nint(lai/2.0*100.)
        else
         output(17)=100
        end if

        if (dom.eq.0) then 
         output(18)=0.0
        else
         output(18)=nint(pftpar(dom,6)*100.)  !root percent
        end if      
c       Store monthly fpar values in positions 25-36:
        do 12 month=1,12
         output(24+month)=optdata(dom,24+month)
 12     continue

c      Annual mean delta 13C stored in position 50-51
       output(50)=real(optdata(dom,50))/10. !total deltaA
       output(51)=optdata(dom,51)  !average deltaA for mixed ecosystems
       output(52)=optdata(8,52)    !phi value

c------------------------------------
c     changed to NPP for temporary output

       output(60)=optnpp(1)   !optimized NPP
       output(61)=optnpp(2)   !for all PFTs
       output(62)=optnpp(3)  
       output(63)=optnpp(4)
       output(64)=optnpp(5)
       output(65)=optnpp(6)
       output(66)=optnpp(7)
       output(67)=optnpp(8)
       output(68)=optnpp(9)
       output(69)=optnpp(10)  
       output(70)=optnpp(11)

c------------------------------------

c       output(60)=optdata(1,50)  !mean C3 photosynthesis discrimination
c       output(61)=optdata(2,50)  !for all PFTs
c       output(62)=optdata(3,50)  
c       output(63)=optdata(4,50)
c       output(64)=optdata(5,50)
c       output(65)=optdata(6,50)
c       output(66)=optdata(7,50)
c       output(67)=optdata(8,50)
c       output(68)=optdata(9,50)
c        
c       output(69)=optdata(8,51)  !mean C4 photosynthesis discrimination
c       output(70)=optdata(9,51)  !for grasses

c------------------------------------

       do pos=80,91
        output(pos)=optdata(dom,pos)  !monthly discrimination for one pft
       end do

       do pos=37,48
        output(pos)=optdata(dom,pos)  !monthly npp, one pft
       end do

       do pos=101,112
        output(pos)=optdata(dom,pos)  !monthly delta e, dominant PFT
       end do

       do pos=113,124
        output(pos)=optdata(dom,pos)  !monthly het resp, dom pft
       end do

       do pos=125,136
        output(pos)=optdata(dom,pos)  !monthly isoresp (product)
       end do

       do pos=137,148
        output(pos)=optdata(dom,pos)  !monthly net C flux (npp-resp)
       end do

       do pos=160,172
        output(pos)=optdata(dom,pos)  !monthly mean gc
       end do

       do pos=173,184
        output(pos)=optdata(dom,pos)  !monthly LAI
       end do

       do pos=185,196
        output(pos)=optdata(dom,pos)  !monthly runoff
       end do

       output(149)=optdata(dom,149)   !annual NEP
       output(150)=optdata(dom,150)   !annual mean A/g

       output(97)=optdata(dom,97)  !Mean annual hetresp scalar
       output(98)=optdata(dom,98)  !pct of NPP that is C4
       output(99)=optdata(dom,99)  !annual het resp

       output(199)=optdata(wdom,199) !firedays
       output(200)=optdata(dom,200) !greendays

       do pos=201,241
        output(pos)=optdata(dom,pos) !ten-day lai*100
       end do

c      Monthly soil moisture, mean, top, and bottom layers *100

       open(37,file='hydro.dat',status='unknown')
       
       do pos=389,400
        output(pos)=optdata(dom,pos-376)  !mean
       end do

       do pos=413,424
        output(pos)=optdata(dom,pos)      !top
       end do

       do pos=425,436
        output(pos)=optdata(dom,pos)      !bottom
       end do

       do month=1,12
        write(37,*)month,output(412+month),output(424+month),dom
       end do
       
       output(425)=nint(wetlayer(dom,1))
       output(426)=nint(wetlayer(dom,2))
       output(427)=nint(wetratio(dom)*100.)
       
       output(450)=optdata(dom,450)    !meanKlit
       output(451)=optdata(dom,451)    !meanKsoil
       output(452)=tcm                     !coldest monrh temp
       output(453)=gdd0                  !gdd0
       output(454)=gdd5                !gdd5

c---------------------------------------------------------------------

      return

      end

c*******************************************************************************
      subroutine newassignbiome
     >(optpft,woodpft,grasspft,subpft,optnpp,woodnpp,grassnpp,subnpp,
     > greendays,biome,gdd0,gdd5,tcm,present,woodylai,grasslai,tmin)

c     this is a new subroutine for assigning biomes in BIOME3.5
c     according to a new scheme of biomes.
c     Jed Kaplan 3/1998

      implicit none

      integer biome,greendays
      integer optpft,woodpft,grasspft,subpft

      real optnpp,woodnpp,grassnpp,subnpp,nppdif,gdd0,gdd5,tcm
      real woodylai,grasslai,tmin
      
      logical present(14)

c     list of the 28 biomes assigned, including land ice
c-------------------------------------------------------
c     1  Tropical evergreen forest
c     2  Tropical semi-deciduous forest
c     3  Tropical deciduous forest/woodland
c     4  Temperate deciduous forest
c     5  Temperate conifer forest
c     6  Warm mixed forest
c     7  Cool mixed forest
c     8  Cool conifer forest
c     9  Cold mixed forest
c     10 Evegreen taiga/montane forest
c     11 Deciduous taiga/montane forest
c     12 Tropical savanna
c     13 Tropical xerophytic shrubland
c     14 Temperate xerophytic shrubland
c     15 Temperate sclerophyll woodland
c     16 Temperate broadleaved savanna
c     17 Open conifer woodland
c     18 Boreal parkland
c     19 Tropical grassland
c     20 Temperate grassland
c     21 Desert
c     22 Steppe tundra
c     23 Shrub tundra
c     24 Dwarf shrub tundra
c     25 Prostrate shrub tundra
c     26 Cushion forb lichen moss tundra
c     27 Barren
c     28 Land ice
c-------------------------------------------------------

c-----barren-------------------------
      if (optpft.eq.0) then
       biome=27
       goto 200
      end if
c------------------------------------

c-----arctic/alpine biomes-----------
      if (optpft.eq.13) then
       biome=26    !cushion-forb tundra 
       goto 200
      end if

      if (optpft.eq.11) then
        if (gdd0.lt.200.0) then
         biome=25  !prostrate shrub tundra
         goto 200
        else if (gdd0.lt.500.0) then
         biome=24  !dwarf shrub tundra 
        else
         biome=23  !shrub tundra
        end if 
       goto 200
      else if (optpft.eq.12) then
       biome=22    !steppe-tundra
       goto 200
      end if
c------------------------------------

c-----desert-------------------------
      if (optpft.eq.10) then
       if (grasslai.gt.1.0) then
        if (tmin.ge.0.0) then
         biome=13
         goto 200
        else
         biome=14
         goto 200
       end if
       else
        biome=21
        goto 200
       end if
      else if (optnpp.le.100.0) then
       if (optpft.le.5.or.optpft.eq.9.or.optpft.eq.10) then
        biome=21
        goto 200
       else if (optpft.eq.8) then
        if (subpft.ne.6.or.subpft.ne.7) then
         biome=21
         goto 200
        end if
       end if
      end if
c------------------------------------

c-----boreal biomes------------------
      if (optpft.eq.6) then
       if (gdd5.gt.900.0.and.tcm.gt.-19.0) then
        if (present(4)) then
        biome=7
       else
         biome=8
       end if 
       else
        if (present(4)) then
        biome=9
       else
         biome=10
       end if 
       end if
       goto 200
      end if

      if (optpft.eq.7) then
       if (subpft.eq.4) then
        biome=4
        goto 200
       else if (subpft.eq.5) then  !.or.subpft.eq.6
        biome=9
        goto 200
       else if (gdd5.gt.900.0.and.tcm.gt.-19.0) then
        biome=9
        goto 200
       else
        biome=11
        goto 200
       end if
      end if       
c------------------------------------

c-----temperate biomes---------------
      nppdif=optnpp-subnpp

      if (optpft.eq.8) then
       if (gdd0.ge.800.0) then
        biome=20 
        goto 200
       else 
        biome=22
        goto 200
       end if
      end if

      if (optpft.eq.3) then
       biome=6
       goto 200
      end if

      if (optpft.eq.4) then
       if (present(6)) then
        if (tcm.lt.-15.) then
        biome=9 !cold mixed
       else
        biome=7 !cool mixed
       end if
       goto 200
       else if 
     >  (present(3).or.
     >  (present(5).and.gdd5.gt.3000.0.and.tcm.gt.3.0))  !tmin -21?
     > then
        biome=6
        goto 200
       else
        biome=4
        goto 200
       end if
      end if

      if (optpft.eq.5) then
       if (present(3)) then 
        biome=6
        goto 200
       else if (subpft.eq.4.and.nppdif.lt.50.) then
        biome=5
        goto 200
       else if (subpft.eq.7) then
        biome=9
        goto 200
       else
        biome=5
        goto 200
       end if
      end if

c------------------------------------
c-----savanna and woodland-----------
      if (optpft.eq.14) then
       if (woodpft.le.2) then
        if (woodylai.gt.4.0) then
         biome=12  !tropical savanna
        else       
         biome=13  !tropical xero scrub 
       end if
        goto 200
       else if (woodpft.eq.3) then
        biome=15   !sclerophyll woodland
        goto 200
       else if (woodpft.eq.4) then
        biome=16   !temp brdlf savanna
        goto 200
       else if (woodpft.eq.5) then
        biome=17   !open conifer
        goto 200
       else if (woodpft.eq.7.or.woodpft.eq.6) then
        biome=18   !boreal parkland
        goto 200
       end if
      end if
c------------------------------------

c-----tropical biomes----------------
      if (optpft.le.2.or.optpft.eq.9) then

       if (optpft.eq.1) then
        biome=1
        goto 200
       end if

       if (optpft.eq.2) then
        if (greendays.gt.300) then
         biome=1
         goto 200
        else if (greendays.gt.250) then
         biome=2
         goto 200
        else
         biome=3
         goto 200
        end if
       end if

       if (optpft.eq.9) then
        biome=19
        goto 200
       end if

      end if
c------------------------------------
      
      biome=0

200   return

      end

c*************************************************************************
c      Run npp optimization model for one pft:
   
       subroutine findnpp(pfts,pft,optlai,optnpp,wst,dtemp,sun,
     >  temp,dprec,dmelt,dpet,dayl,k,pftpar,optdata,dphen,co2,p,tsoil,
     >  realout,numofpfts)

       implicit none
                                   
       integer numofpfts
       integer pft,i,pfts(numofpfts),optdata(0:numofpfts,500)
       integer inv(500),iterate

       real temp(12),sun(12),dayl(12),tsoil(12)
       real dprec(365),dphen(365,2)
       real dpet(365),co2,p,realout(0:numofpfts,200),realin(200)
       real optnpp,optlai,dtemp(365)
       real wst,k(12),pftpar(25,25)
       real npp,dmelt(365)
       real lowbound,range,alai(2)

c      Set output values to zero:
       do i=1,500
        optdata(pft,i)=0
       end do

       optnpp = 0.
       optlai = 0.

c      If pft.ne.0 this is a dummy call of subroutine, return zero values:
       if(pfts(pft).ne.1) then
        return
       end if

c       print*,'entered findnpp',pft

c---------------------------------------------------------------------      
     
c      Calculate NPP at a range of different leaf areas by iteration:

      lowbound=0.01
      range=8.0

      do iterate=1,8

       alai(1)=lowbound+(1./4.)*range
       alai(2)=lowbound+(3./4.)*range

       call growth(npp,alai(1),wst,sun,temp,dprec,dmelt,dpet,
     > k,pftpar,pft,dayl,dtemp,inv,dphen,co2,p,tsoil,realin)

        if (npp.ge.optnpp) then
         optlai = alai(1)
         optnpp = npp
         do i=1,500
          optdata(pft,i)=inv(i)
c          realout(pft,i)=realin(i)
         end do
        end if

       call growth(npp,alai(2),wst,sun,temp,dprec,dmelt,dpet,
     > k,pftpar,pft,dayl,dtemp,inv,dphen,co2,p,tsoil,realin)

c       Find the leaf area which gives the highest NPP:
        if (npp.ge.optnpp) then
         optlai = alai(2)
         optnpp = npp
         do i=1,500
          optdata(pft,i)=inv(i)
c          realout(pft,i)=realin(i)
         end do
        end if

       range=range/2.0
       lowbound=optlai-range/2.0
       if (lowbound.le.0.0) lowbound=0.01

      end do

c---------------------------------------------------------------------      

20     return

       end
c***************************************************************************
c      Subroutine growth calculates NPP of one PFT
 
       subroutine growth(npp,maxlai,annp,sun,temp,dprec,dmelt,
     >  dpet,k,pftpar,pft,dayl,dtemp,outv,dphen,co2,p,tsoil,realin)

       implicit none

       integer pft,m,month,j,days(12),phentype!,midday(12)
       integer outv(500),grass,i,c4months,greendays,day,mcount

       logical c4,c4month(12),wilt

       real dprec(365),dpet(365),annp,npp,p
       real gpp,dayl(12),pftpar(25,25),maxfvc
       real sun(12),temp(12),root,c4pot
       real k(12),age,emax
       real meangc(12),realin(200)
       real mgpp(12),gphot
       real rainscalar,wst,doptgc(365),optgc(12)
       real dtemp(365),dphen(365,2)
       real xmid,aday,dx,fmid
       real igphot,gt,meanfvc(12),dmelt(365)
       real ap,fpar,tsecs,meanwr(12,3)
       real rtbis,x1,x2,annaet,co2,ca
       real pgphot,leafresp,mlresp(12),leaftime,maxgc
       real alresp,mgmin,lresp(12),fr,runoff,runoffmo(12)
       real optratio,kk(13),stemresp,maxlai,optratioa(13)
       real annualapar,annualparr,annualfpar
       real monthlyfpar(12),monthlyparr(12),monthlyapar(12)
       real backleafresp(12)

       real CCratio(12),isoresp(12),phi,isoC3,isoC4,dayfvc(365)
       real C3DA(12),c4gpp(12),c4pct,annresp,annnep,C4DA(12)
       real maintresp(12),mstemresp(12),mrootresp(12),mgrowresp(12)
       real mnpp(12),meanaet(12),tsoil(12),cflux(12)
       real rlit(12),rfst(12),rslo(12),riso(12),rtot(12),riflux(12)
       real wet(365),firedays,annc4npp,c4ccratio(12),c4leafresp(12)
       real c4fpar(12),c4parr(12),c4apar(12),monthlylai(12)
       real tendaylai(40),totnpp,c4mnpp(12),nppsum,anngasum,Rmean

       real meanKlit,meanKsoil

c       data (midday(m),m=1,12)
c     *   / 16,44,75,105,136,166,197,228,258,289,319,350 /

       data (days(month),month=1,12) 
     *   /  31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.  /

c      This array defines pft-specific maximum Ci/Ca ratios.
       data (optratioa(i),i=1,13)
     >/0.95,0.9, 0.8,0.8,0.9, 0.8,0.9, 0.65,0.65, 0.70, 0.90,0.75,0.80/

       data (kk(i),i=1,13)
     >  / 0.7,0.7,0.6,0.6,0.5,0.5,0.4,0.4,0.4,0.3,0.5,0.3,0.6 /

c---------------------------------------------------------------------------
       ca=co2*1e-6
c---------------------------------------------------------------------------
c      Initialize day one value for soil moisture
c      COULD MAYBE DO THIS AS A FUNCTION OF AET/PET! 
       rainscalar=1000.
       wst = annp / rainscalar
       if(wst.ge.1.)  wst=1.
c---------------------------------------------------------------------------
c      Assign pft specific parameters for photosynthesis model 

       phentype  = nint(pftpar(pft,1))
       mgmin     = pftpar(pft,2)
       root      = pftpar(pft,6)
       age       = pftpar(pft,7)
       c4pot     = pftpar(pft,11) 
       grass     = nint(pftpar(pft,10))
       emax      = pftpar(pft,3)
c----------------------------------------------------------------------------
c      Calculate the maxfvc from the maxlai and (fixed) k value
c       kk=0.5
       maxfvc=1.-exp(-kk(pft)*maxlai)

c------------------------------------------------------------------------    
c      Set the value of optratio depending on whether c4 plant or not.

c       if (pft.eq.8.or.pft.eq.9.or.pft.eq.10) then
       if (pft.eq.9.or.pft.eq.10) then
        c4=.true.
       else
        c4=.false.
       endif

 100   continue

       if (c4) then
        optratio=0.4
       else
        optratio=optratioa(pft)  !assigns the pft-specific ratio
       endif

c----------------------------------------------------------------------------
c      Calculate monthly values for the optimum non-water-stressed gc (optgc)
       
      maxgc=0.
      do 120 month=1,12
      m=month
      tsecs=3600.*dayl(m)

c------------------------------------------------------------------------
c      First find the gc value resulting from the max ci/ca ratio
    
c      Find mid-monthly-day daily tstressed photosynthesis value
       fpar = 1.-exp(-kk(pft)*maxlai)

       if (c4) then
        call c4photo(optratio,sun(m),dayl(m),temp(m),
     >  age,lresp(m),pgphot,aday,fpar,p,ca,pft)
       else
        call photosynthesis(optratio,sun(m),dayl(m),temp(m),
     >  age,lresp(m),pgphot,aday,fpar,p,ca,pft)
       end if

c     changed to include possible aday zero
c     Calculate gt using the physical eqn from aday and ci/ca ratio
      if (tsecs.gt.0.and.aday.gt.0.0) then
       gt = mgmin + ( (1.6*aday) / (ca*(1.-optratio)) ) / tsecs
      else
       gt=0.0
      end if
   
c      This gives us the final non-water-stressed gc value
       optgc(m) = gt

c      Store output values:
       if(maxgc.le.optgc(m)) maxgc=optgc(m)

 120   continue
c-------------------------------------------------------------------------
c      Calculate water balance and phenology for this pft/s and fvc/s
c      Subroutine hydrology returns monthly mean gc & summed fvc value
   
c      Linearly interpolate the mid-month optgc & ga values to daily values
       call daily(optgc,doptgc)
    
       call hydrology
     > (dprec,dmelt,dpet,root,k,maxfvc,pft,phentype,wst,
     >  doptgc,meangc,meanfvc,meanwr,meanaet,annaet,mgmin,dphen,dtemp,
     >  grass,runoff,runoffmo,wet,greendays,dayfvc,emax,
     >  wilt,pftpar)      
c-------------------------------------------------------------------------
c     Now use the monthly values of fvc & meangc to calculate net & gross
c     photosynthesis for an "average" day in the month and multiply by the
c     number of days in the month to get total monthly photosynthesis.

      alresp   = 0.
      gpp      = 0.
      leaftime = 0.
      annualparr=0.
      annualapar=0.

c-------------------------------------------------------------------------      
      do month=1,12                                   !begin monthly loop here
      m=month

      !c4gpp(m)=0.0

c     If meangc is zero then photosynthesis must also be zero
      if(meangc(m).eq.0.)then
       gphot=0.
       rtbis=0.
       leafresp=lresp(m)*(meanfvc(m)/maxfvc)

      else   
c     Iterate to a solution for gphot given this meangc value!
c....................................................................
c      This is a tailored implementation of the bisection method
c      with a fixed 8 bisections and assuming root is bracketed and 
c      that f(x1)<0 and f(x2)>0   

       x1=0.02
       x2=optratio+0.05
       rtbis=x1
       dx=x2-x1
       do 30 j=1,10
        dx=dx*0.5
        xmid=rtbis+dx
c...................................................
c      Evaluate fmid=Anetdt-Ap at the point ci/ca = xmid     
              
       fpar = meanfvc(m)

       if (c4) then
        call c4photo(xmid,sun(m),dayl(m),temp(m),
     >  age,leafresp,igphot,aday,fpar,p,ca,pft)
       else
        call photosynthesis(xmid,sun(m),dayl(m),temp(m),
     >  age,leafresp,igphot,aday,fpar,p,ca,pft)
       end if
  
       gt = 3600.*dayl(m)*meangc(m)

       if (gt.eq.0.0) then
        ap=0.0
       else
        ap = mgmin + (gt/1.6)*(ca*(1.-xmid))
       end if

       fmid = aday - ap
c....................................................
c      If fmid is closer to the root store new values
       if(fmid.le.0.)then
        rtbis=xmid    
        gphot = igphot
       endif 
 30    continue
c....................................................................        
       endif
      
c     We already include the albedo in the calculation of the net 
c     short wave radiation so apar here really is the absorbed PAR:
c     Idea here is that annualfpar should be the total amount of PAR
c     ansorbed during the year divided by the total amount of PAR per
c     unit area.
c     Get PAR in units of MJ.m-2.month-1 (hence *1e-6)

      monthlyfpar(m) = meanfvc(m)
      monthlyparr(m) = sun(m)*days(m)*1e-6
      monthlyapar(m) = monthlyparr(m)*monthlyfpar(m)
      annualapar  = annualapar + monthlyapar(m) 
      annualparr  = annualparr + monthlyparr(m)

c     Monthly gross photosynthesis (=numdays*average-daily-photosynthesis)
      mgpp(m) = days(m)*gphot
      gpp     = gpp + mgpp(m)

c     Calculate monthly leaf respiration (=numdays*average-daily-leafresp)
      mlresp(m) = days(m)*leafresp
      alresp    = alresp + mlresp(m)    

c     store monthly values of Ca/Cst and leaf resp.

      CCratio(m) = rtbis
      isoresp(m) = mlresp(m)

      if (c4) then
       c4gpp(m)=mgpp(m)            !store monthly c4 values here
       c4fpar(m)=monthlyfpar(m)
       c4parr(m)=monthlyparr(m)
       c4apar(m)=monthlyapar(m)
       c4ccratio(m)=CCratio(m)
       c4leafresp(m)=isoresp(m)
      end if

      end do                                              !the loop ends

c--------------------------------------------------------------------------
c     Calculate monthly LAI

      do m=1,12
       monthlylai(m)=(log(1-monthlyfpar(m)))/(-1.*kk(pft))
      end do

c     And ten-day lai
      i=1
      do day=1,365,10
       tendaylai(i)=(log(1-dayfvc(day)))/(-1.*kk(pft))
       i=i+1
      end do

c---------------------------------
c     Calculate annual FPAR (%) from annual totals of APAR and PAR

      if (annualapar.eq.0.) then
       annualfpar=0.0
      else
       annualfpar=100.*annualapar/annualparr
      end if

c---------------------------------------

c     Calculate annual respiration costs to find annual npp:
      call respiration
     >(npp,gpp,alresp,temp,grass,maxlai,stemresp,fr,
     >mstemresp,mrootresp,pft,mlresp,monthlyfpar,backleafresp)

c     If the plant wilted during the year, it is dead for this LAI.

      if (wilt) npp=-9999.0

c---------------------------------------
c     calculate monthly NPP

      nppsum=0.0

      do m=1,12
       mnpp(m)=0.0
      end do

      do m=1,11
       maintresp(m)=
     > mlresp(m)+backleafresp(m)+mstemresp(m)+mrootresp(m)
       mgrowresp(m) = (0.02*(mgpp(m+1)-maintresp(m+1)))
        if (mgrowresp(m).lt.0.0) mgrowresp(m)=0.0
       mnpp(m) = mgpp(m)-(maintresp(m)+mgrowresp(m))
      end do
      
      maintresp(12)=
     >mlresp(12)+backleafresp(12)+mstemresp(12)+mrootresp(12)
      mgrowresp(12) = (0.02*(mgpp(1)-maintresp(1)))
       if (mgrowresp(12).lt.0.0) mgrowresp(12)=0.0
      mnpp(12) = mgpp(12)-(maintresp(12)+mgrowresp(12))

      do m=1,12
       if (c4) c4mnpp(m) = mnpp(m)
       nppsum=nppsum+mnpp(m)
      end do

c----------------------------------------------------------------------

c     If this is a temperate grass or desert shrub pft, compare both
c     C3 and C4 NPP and choose the more productive one on a monthly basis. 
c     However the C4 advantage period must be for at least two months 
c     (ie. long enough to complete a life cycle).

c      if (pft.eq.8.or.pft.eq.10) then
      if (pft.eq.10) then
       if (c4) then
        c4=.false.
        goto 100       !compute everything again, with C3 pathway 
       end if
      end if
  
      c4months=0
      annc4npp=0.0

      do m=1,12
       if (pft.eq.9) then
        c4month(m)=.true.
        
       else
        c4month(m)=.false.
       end if
      end do

c      if (pft.eq.8.or.pft.eq.10) then
      if (pft.eq.10) then

       do m=1,12
        if (c4mnpp(m).gt.mnpp(m)) c4months=c4months+1
       end do

       if (c4months.ge.3) then
        do m=1,12
         if (c4mnpp(m).gt.mnpp(m)) then
          c4month(m)=.true.
         else
          c4month(m)=.false.
         end if
        end do
       end if

      end if

      totnpp=0.0

      do m=1,12
       if (c4month(m)) then

        mnpp(m)=c4mnpp(m)
        annc4npp=annc4npp+c4mnpp(m)
        totnpp=totnpp+mnpp(m)

        monthlyfpar(m)=c4fpar(m)
        monthlyparr(m)=c4parr(m)
        monthlyapar(m)=c4apar(m)
        CCratio(m)=c4ccratio(m)
        isoresp(m)=c4leafresp(m)
       else
        totnpp=totnpp+mnpp(m)
       end if
      end do

      if (c4months.ge.2) nppsum=totnpp

c     Calculate % of annual npp that is C4
      c4pct=annc4npp/npp

c---------------------------------------------------------------------------

      if (npp.ne.nppsum) npp=nppsum

      if (npp.le.0.0) then
       return
      else
       continue
      end if

c---------------------------------------------------------------------------

      if (gpp.gt.0.0) then

c     calculate the phi term that is used in the C4 13C fractionation
c     routines
      if (pft.ge.8) then
       call calcphi(mgpp,phi)
      end if

c     calculate carbon isotope fractionation in plants
      call isotope(CCratio,ca,temp,isoresp,c4month,mgpp,phi,
     >isoC3,isoC4,C3DA,C4DA,gpp)

      end if

c----------------------------------------------------------------------
c     Call subroutine to calculate heterotrophic respiration
      call hetresp
     >(pft,npp,temp,tsoil,meanaet,meanwr,rlit,rfst,rslo,rtot,isoC3,riso,
     >riflux,Rmean,meanKlit,meanKsoil)

      annresp=0.0   !zero the annual hetresp calculation
      do m=1,12    !sum up monthlies to get an ann hetresp = npp
       annresp=annresp+rtot(m)
      end do
c      write(*,*)annresp

      annnep=0.0
c     calculate monthly ecosystem carbon flux NPP-Hetresp
      do m=1,12
       cflux(m)=mnpp(m)-rtot(m)
       annnep=annnep+cflux(m)
      end do

c-----------------------------------------------
c     Call fire subroutine
      call fire (wet,pft,maxlai,npp,firedays)
      
c--------------------------------------------------------------------------
c     Outputs section:
     
c     Output the monthly soil moisture value for this pft and lai:   
      do m=1,12

       outv(12+m)=nint(100.*meanwr(m,1))
       outv(412+m)=nint(100.*meanwr(m,2))
       outv(424+m)=nint(100.*meanwr(m,3))

       outv(24+m)=nint(100.*monthlyfpar(m))
      end do

c     Record values of output variables:
      outv(1) = nint(npp)                
      outv(3) = nint(annaet)
      outv(4) = nint(maxgc)
      outv(5) = nint(stemresp)
      outv(6) = nint(runoff)
      outv(7) = nint(annualparr)
      outv(8) = nint(annualfpar)
      outv(9) = nint(fr)
      outv(50)= nint(isoC3*10.)
      outv(51)= nint(isoC4*10.)
      outv(52)= nint(phi*100.)
      outv(97)= nint(Rmean*100.)
      outv(98)= nint(c4pct*100.)
      outv(99)= nint(annresp*10.)

      anngasum=0.0
      mcount=0

      do m=1,12
       realin(36+m)=mnpp(m)
       outv(79+m)=nint(C3DA(m)*100.)    !includes c3 and c4
       outv(36+m)=nint(mnpp(m)*10.)
c       outv(100+m)=nint(mlresp(m)*10.)
       outv(100+m)=nint(riso(m)*10.)
       outv(112+m)=nint(rtot(m)*10.)
       outv(124+m)=nint(riflux(m)*10.)
       outv(136+m)=nint(cflux(m)*10.)
       outv(160+m)=nint(meangc(m))
       outv(172+m)=nint(monthlylai(m)*100.)
       outv(184+m)=nint(runoffmo(m))
       
       if (meangc(m).ne.0) then
        mcount=mcount+1
        anngasum=anngasum+(mgpp(m)/meangc(m))  !new line for A/g!
       end if
       
      end do
      
      outv(150)=nint((anngasum/mcount)*100.)            !new line for A/g!

      outv(149)=nint(annnep*10.)

      outv(199)=nint(firedays)
      outv(200)=greendays

      do i=1,40
       outv(200+i)=nint(tendaylai(i)*100.)
      end do
      
      outv(450)=nint(meanKlit*100.)
      outv(451)=nint(meanKsoil*100.)
      
c------------------------------------------------------------------------
      return

      end

c***************************************************************************
c      C3 Photosynthesis subroutine:
   
       subroutine photosynthesis(ratio,dsun,daytime,temp,
     *  age,leafresp,grossphot,aday,fpar,p,ca,pft)

       implicit none      

       integer pft,n

       real dsun,temp,daytime
       real c1,c2,s,teta,drespc3,qeffc3
       real ko,kc,ts,abs1,tao,vmax
       real ko25,kc25,tao25,koq10,kcq10,taoq10
       real p,pi,o2,jtoe,cmass,kk
       real tstress,ca,t0(13)
       real z,aday,grossphot,fpar
       real oc,ratio,age,leafresp,tune,optratio
       real je,jc,wif,adaygc,twigloss
       real slo2,mfo2
       real leafcost,tcurve(13),mintemp

       parameter(qeffc3=0.08)
       parameter(drespc3=0.015)
       parameter(abs1=1.,teta=0.7)
       parameter(slo2=20.9*1e3,jtoe=2.3*1e-6,optratio=0.95)  !figure this out
       parameter(ko25=30.*1e3,kc25=30.,tao25=2600.,cmass=12.)
       parameter(kcq10=2.1,koq10=1.2,taoq10=0.57,twigloss=1.)
                         
c      t0 defines the minimum mean monthly temperature
c      at which photosynthesis takes place

       data (t0(n),n=1,13)
     >   /10.,10., 5.,4.,3., 0.,0., 4.5,10., 5., -7.,-7.,-12./

       data (tcurve(n),n=1,13)
     >   /1.0,1.0, 1.0,1.0,0.9, 0.8,0.8, 1.0,1.0, 1.0, 0.6,0.6,0.5 /

c      Twigloss assumes some % of FPAR is lost to absorption by stems
c      A tune of 50% accounts for all losses!
       parameter(tune=1.0)

c      The leafcost parameter is related to expected leaf longevity

       leafcost =(age/12.)**0.25

c      Need to change the partial pressure of o2 also:

       mfo2=slo2/1e5
       o2=p*mfo2 

c      If daytime<=1 hour set to one hour to avoid /0.
       if(daytime.le.4.) daytime=4.

c---------------------------------------------------------------------------
c      tstress limits quantum efficiency as a function of temperature
c      Because of temperature optimization (and maybe nitrogen limitations)
c      this value is PFT specific.

        mintemp=t0(pft)
        if (temp.gt.mintemp+0.1) then   !the +0.1 is here to prevent underflows
         tstress=tcurve(pft)*exp(-10.0/(temp-mintemp))
        else
         tstress=0.0
        endif

c--------------------------------------------------------------------
c      work out temperature adjusted values of parameters
       ko  =    ko25*koq10**((temp-25.)/10.)
       kc  =    kc25*kcq10**((temp-25.)/10.)
       tao =  tao25*taoq10**((temp-25.)/10.)

c      Set non-co2-dependent parameters
       s  = drespc3*(24./daytime)
       ts = o2 / (2.*tao)
       kk = kc*(1. + (o2/ko)) 
       z  = cmass*jtoe*dsun*fpar*twigloss*tune

c--------------------------------------------------------------------
c      First calculate the vm value based on a ratio=0.95

       pi = optratio*ca*p
       c1 = tstress*qeffc3*( (pi-ts)/(pi+2.*ts) )       
       c2 = (pi - ts) / (pi + kk) 
       oc = ( (s-teta*s)/(c2-teta*s) )**0.5      

c      Estimate the optimal value of Vm at ratio=0.95 g(C).m-2.day-1

       if (z.eq.0.0) then
        vmax=0.0
       else
        vmax = (z/drespc3)*(c1/c2)*((2.*teta-1.)*s-(2.*teta*s-c2)*oc)
       end if

c.........................................................
c      Now use this vm value to calculate actual photosynthesis

c      Calculate inter-cellular co2 partial pressure
       pi = ratio*ca*p 

c      If pi is less than the compensation point then grossphot=0.

       if(pi.le.ts)then
        grossphot=0.0
       else
c      Otherwise calculate grossphot
        c1 = tstress*qeffc3*( (pi-ts)/(pi+2.*ts) )       
        c2 = (pi - ts) / (pi + kk)

        if (z.eq.0.0) then
         je=0.0
        else
         je=c1*z    / daytime
        end if

        if (vmax.eq.0.0) then
         jc=0.0
        else
         jc=c2*vmax / 24.
        end if

        wif = daytime/(2.*teta)

        if (je.eq.0.0.and.jc.eq.0.0) then
         grossphot=0.0
        else
         grossphot=wif*(je+jc-((je+jc)**2.-4.*teta*je*jc)**0.5)
        end if
       endif

c      Calculate net-daytime-photosynthesis and daily dark respiration
       adaygc = grossphot - (daytime/24.)*drespc3*vmax

       leafresp = drespc3*vmax*leafcost
       if (leafresp.lt.0.0) leafresp=0.0

c      Change Aday from gC.m-2.day-1 to umol.day-1
       if (adaygc.eq.0.0) then
        aday=0.0
       else
        aday = (adaygc/cmass)*(8.314*(temp+273.3)/p)*1000.
       end if

       return

       end

c**************************************************************************
c     C4 photosynthesis subroutine:
 
      subroutine c4photo(ratio,dsun,daytime,temp,
     >  age,leafresp,grossphot,aday,fpar,p,ca,pft)

       implicit none      

       integer pft

       real dsun,temp,daytime
       real c1,c2,s,teta
       real ts,abs1
       real ko25,kc25,tao25,koq10,kcq10,taoq10,tao
       real p,pi,jtoe,cmass
       real tstress,ca,t0(10)
       real z,aday,grossphot,fpar
       real oc,ratio,age,leafresp,optratio
       real je,jc,wif,twigloss
       real qeffc4,adaygcc4,grossphotc4,drespc4
       real adayc4,leafrespc4,vmaxc4
       real damage,leafcost,mintemp,maxtemp,tune
       real o2,mfo2,slo2

       parameter(drespc4=0.03)
       parameter(abs1=1.,teta=0.7)
       parameter(slo2=20.9*1e3, jtoe=2.3*1e-6, optratio=0.95)
       parameter(ko25=30.*1e3,  kc25=30.,tao25=2600., cmass=12.)
       parameter(kcq10=2.1, koq10=1.2, taoq10=0.57, twigloss=1.)

c      This leafcost parameter is a way of making the respiration costs
c      of evergreen leaves larger than deciduous leaves.

       leafcost=(age/12.0)**0.25

       t0(8)  = 10.0
       t0(9)  = 10.0
       t0(10) = 10.0

c      C4 quantum efficiency is a function of actual photosynthetic pathway
c      (NAD-me or NADP-me) and plant functional type (dicot or monocot).
c      Here we use a mean between the pathways for both woody and grass types.

       if (pft.eq.8.or.pft.eq.9) then
        qeffc4=0.0633
        tune=1.0
       else if (pft.eq.10) then
        qeffc4=0.0565
        tune=0.75     !for widely spread leaves and stems
       end if
c---------------------------------------------------------------------------
c      tstress limits quantum efficiency as a function of temperature
c      Because of temperature optimization (and maybe nitrogen limitations)
c      this value is PFT specific.

        mintemp=t0(pft)
        maxtemp=55.0
        if (temp.gt.mintemp+0.1.and.temp.lt.maxtemp) then
         tstress=exp(-10.0/(temp-mintemp))
        else
         tstress=0.0
        endif

        if (tstress.gt.1.0) tstress=1.0

c--------------------------------------------------------------------
c      work out temperature adjusted values of parameters
       tao =  tao25*taoq10**((temp-25.)/10.)

c      Set non-co2-dependent parameters
       ts = o2 / (2.*tao)
       z  = cmass*jtoe*dsun*fpar*twigloss*tune

c      Need to change the partial pressure of o2 also:
       mfo2=slo2/1e5
       o2=p*mfo2 

c--------------------------------------------------------------------
       pi = optratio*ca*p
       s  = drespc4*(24./daytime)
       c1 = qeffc4*tstress
       c2 = 1.
       oc = ( (s-teta*s)/(c2-teta*s) )**0.5

c      Estimate the optimal value of Vm at ratio=0.8 g(C).m-2.day-1
       if (z.eq.0.0) then
        vmaxc4 = 0.0
       else
        vmaxc4 = (z/drespc4)*(c1/c2)*((2.*teta-1.)*s-(2.*teta*s-c2)*oc)
       end if
c.........................................................
c      Now use this vm value to calculate actual photosynthesis

c      If pi is less than the compensation point then grossphot=0.
       if(pi.le.ts)then
        grossphotc4=0.0
       else
c      Otherwise calculate grossphot
        c1 = qeffc4*tstress
        c2 = 1.

        if (z.eq.0.0) then
         je=0.0
        else
         je=c1*z / daytime
        end if
     
        if (vmaxc4.eq.0.0) then
         jc=0.0
        else
         jc=c2*vmaxc4 / 24.
        end if

c       Damage gives the limitation of c4 photosynthesis by pi
        if(ratio.lt.0.4)then
         damage = ratio/0.4
        else
         damage = 1.
        endif
        wif = damage*daytime/(2.*teta)
  
        if (je.eq.0.0.and.jc.eq.0.0) then
         grossphotc4=0.0
        else
         grossphotc4=wif*(je+jc-((je+jc)**2.-4.*teta*je*jc)**0.5)
        end if

       endif

c      Calculate net-daytime-photosynthesis and daily dark respiration
       adaygcc4 = grossphotc4 - (daytime/24.)*drespc4*vmaxc4
       leafrespc4 = drespc4*vmaxc4*leafcost

c      Change Aday from gC.m-2.day-1 to mm.day-1
       if (grossphotc4.eq.0..and.vmaxc4.eq.0.)then
        adayc4=0.0
       else
        adayc4 = (adaygcc4/cmass)*(8.314*(temp+273.3)/p)*1000.
       end if

       aday=adayc4
       leafresp=leafrespc4
       grossphot=grossphotc4

       return
       end

c**************************************************************************
c      Subroutine to calculate annual respiration costs
 
       subroutine respiration(npp,gpp,alresp,temp,grass,lai,
     >  stemresp,percentcost,mstemresp,mrootresp,pft,mlresp,
     >  fpar,backleafresp)

       implicit none

       integer m,grass,pft

       real npp,gpp,alresp,temp(12),lai,mlresp(12)
       real growthresp,stemresp,leafresp,finerootresp
       real minallocation,ln,percentcost!,days(12)
       real litterfall,stemcarbon,y,p1,e0,tref,m10
       real mstemresp(12),mrootresp(12),t0,respfact(13)
       real allocfact(13)
       real fpar(12),backleafresp(12),leafmaint

c       data (days(m),m=1,12) 
c     *   /  31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.  /

c      Grass defines if there is sapwood respiration

c      Ln  = Leaf litterfall per unit Leaf area index
c      m10 = Maintenance respiration of sapwood at 10oC, gC.month-1.KgC-1
c      k   = Extinction coefficient
c      y   = Efficiency with which carbon is turned into biomass
c      p1  = Fine root respiration to litterfall ratio

c      stemcarbon = sapwood mass as KgC.m-2(leaf area).m-2(ground area)
       parameter(Ln=50.,y=0.8,m10=1.6,p1=0.25,stemcarbon=0.5) 

c      e0, t0 and tref are parameters from Lloyd & Taylor 1995
       parameter(e0=308.56, tref=10.0,t0=46.02)

c      t0 can be used as a pft specific parameter to modify the shape of
c      the curve describing the temperature dependence of respiration

       data (respfact(m),m=1,13)
     >  /0.8,0.8, 1.4,1.6,0.8, 4.0,4.0, 1.6,0.8, 1.4, 4.0,4.0,4.0/

       data (allocfact(m),m=1,13)
     >  /1.0,1.0, 1.2,1.2,1.2, 1.2,1.2, 1.0,1.0, 1.0, 1.0,1.0,1.5/

c------------------------------------------------------------------------
c      Calculate leafmass (gC.m-2) and litterfall (gC.m-2.year-1)
       litterfall=lai*Ln*allocfact(pft)

c      Calculate stem maintenace respiration costs in gC.year-1 

       stemresp = 0.0

       do m=1,12
        if (temp(m).le.-46.02) then
         mstemresp(m)=0.0
        else
         mstemresp(m) = lai*stemcarbon*respfact(pft)*
     >   exp( e0*(1./(tref+t0) - 1./(temp(m)+t0)) )
        end if
        stemresp = mstemresp(m) + stemresp
       end do

c      Calculate belowground maintenance respiration costs in gC.year-1     

       leafmaint=0.0
       finerootresp = p1*litterfall
       do m=1,12
        mrootresp(m)=(mstemresp(m)/stemresp)*finerootresp
        backleafresp(m)=mrootresp(m)*fpar(m)*4.0
        leafmaint=backleafresp(m)+leafmaint
       end do
   
c      Assume leaf respiration costs supplied are in units of gC.year-1
       leafresp = alresp+leafmaint

c      if this is a grass clear out the stem respiration

       if (grass.eq.2) then
        stemresp=0.0
        do m=1,12
         mstemresp(m)=0.0
        end do
       end if

c      20% of whats left goes to construction respiration, gC.year-1
       growthresp = (1.-y)*(gpp-stemresp-leafresp-finerootresp)

c      Finally calculate the resulting annual NPP, gC.year-1
       npp = gpp -stemresp-leafresp-finerootresp-growthresp     

c-------------------------------------------------------------------------

c      Now calculate the minumum allocation requirement. If NPP<litterfall
c      this lai is not sustainable so set NPP=-9999. for output purposes
       minallocation = 1.*litterfall

       if(npp.lt.minallocation) then
        npp=-9999.0
       end if

c-------------------------------------------------------------------------

c      Find respiration costs as a percentage of GPP
       if(gpp.gt.0.and.npp.ne.-9999.)then
        percentcost = 100.*(gpp-npp)/gpp
       else
        percentcost = 0.
       end if   

       return
       end     

c*************************************************************************

c      Subroutine hydrology
c      calculates the actual values of gc and soil moisture

       subroutine hydrology
     >  (dprec,dmelt,deq,root,k,maxfvc,pft
     >  ,phentype,wst,gcopt,meangc,meanfvc,meanwr,meanaet,annaet,mgmin
     >  ,dphen,dtemp,grass,sumoff,runoffmonth,wet,greendays,dayfvc,emax
     >  ,wilt,pftpar)

       implicit none

       integer month,twice,d,days(12),dayofmonth,phentype,grass
       integer greendays,pft

       real w(2),root,fvc,k(12),aet,r1(2),emax
       real perc,runnoff,wr,meanfvc(12),meangc(12)
       real dprec(365),deq(365),dphen(365,2),gcopt(365)
       real maxfvc,wst,gc,dtemp(365),wet(365),dayfvc(365)
       real annaet,drainage,meanwr(12,3),mgmin,meanaet(12)
       real dmelt(365),supply,alfa,alfam,gm,gsurf,gmin
       real onnw,offw,sumoff,demand,waste,wetphytomass,a
       real evap,runoffmonth(12)

       real pftpar(25,25)

       logical wilt

       parameter(alfam=1.4,gm=5.)
c       parameter(onnw=0.4,offw=0.3)

       onnw=pftpar(pft,4)
       offw=pftpar(pft,4)

       data days / 31,28,31,30,31,30,31,31,30,31,30,31  /
       
c      Initialize soil moisture stores for day one of the "spin-up" year 
       w(1) = wst
       w(2) = wst

c----------------------------------------------------------------------------
c      Run the hydrology and phenology models!
     
c      Run for one "spin-up" year & then use output from the 2nd year 
       do 100 twice = 1,2
  
       d=0
       annaet=0.
       sumoff=0.
       greendays=0
       wilt=.false.

c      Do the daily calculations to find the monthly output values
       do 110 month = 1,12

        meanfvc(month) = 0.
        meangc(month)  = 0.    
        meanwr(month,1)= 0.
        meanwr(month,2)= 0.
        meanwr(month,3)= 0.
       meanaet(month) = 0.0   
       runoffmonth(month)=0.0 

       do 120 dayofmonth = 1,days(month)
       d = d+1

c      Calculate effective soil moisture in rooting zone 
       wr =  root*w(1) + (1.-root)*w(2)

c      deq is the daily PET
c      maxfvc is foliar vegetation cover (related to LAI).
c      phentype is phenological type (1 evergreen, 2 summergreen, 3 watergreen)
c      offw is soil moisture threshold for leaf drop


c      Calculate vegetation phenology for today     

       if(phentype.eq.1)then                   !evergreen    
        fvc = maxfvc                                        

       else if(phentype.eq.2)then              !cold deciduous
        fvc = maxfvc*dphen(d,grass)  

       else if(grass.eq.2)then                 !cold deciduous
        fvc = maxfvc*dphen(d,grass)  

c------new code-02.05.99----
         if(fvc.gt.0.01.and.wr.gt.offw)then  !drought deciduous
          fvc = fvc
        else if(fvc.lt.0.01.and.wr.gt.onnw)then
         fvc = fvc
        else
         fvc = 0.0
        end if
c------end new code---------

       elseif(fvc.gt.0.01.and.wr.gt.offw)then  !drought deciduous
        fvc = maxfvc
       elseif(fvc.lt.0.01.and.wr.gt.onnw)then    
        fvc = maxfvc
       else
        fvc = 0.0
       endif

       if (fvc.gt.0.0) greendays=greendays+1

c------------------------------------------------------------------------
       if(dtemp(d).le.-10.0) then
        gc=0.0
        aet=0.0
        perc=0.0
       else    !begin a loop here to calculate under non-freezing conditions

c---------------------------
c      If the fvc (ie. phenology) indicates vegetation bare of leaves,
c      there can still be some evaporation and loss of water from
c      the soil and stems (25% after Larcher 1995).

       if (fvc.eq.0.0) then
        aet=0.25*deq(d)
       else
c---------------------------

c      Calculate the optimal conductance for today
       gmin = mgmin*fvc
       gc    = gcopt(d)*(fvc/maxfvc)   
       gsurf = gc + gmin 
      
c      Calculate aet from gc & Eq (=deq) using eqn from Monteith 1995
       if (gsurf.gt.0.) then           
        alfa  = min(1., alfam*(1.-exp(-gsurf/gm) ))
        aet   = alfa*deq(d)
       else
        alfa  = 0.
        aet   = 0.
       end if

       end if                 !end phenology sensitive loop


c      Not all soil water, or precip. is effectively used, and some water
c      goes into the new phytomass

       wetphytomass = 0.01*aet
       waste = 0.01*aet

       demand = aet + wetphytomass + waste   

c      Calculate daily supply function in mm/day
       supply = emax*wr

c      If this optimal value of aet is greater than the supply function
c      Then limit aet to the supply function rate.

c      If the gmin cannot be satisfied then the plant wilts (dies) and this
c      LAI is not sustainable.

       if(demand.gt.supply)then
        a = (1. - supply/(deq(d)*alfam))
        if (a.lt.0.0) a=0.0
        gsurf    =  -gm*log(a)
        aet      =  supply
        gc = gsurf - gmin
c       Constrain gc value to zero!
        if (gc.le.0.0) then
         gc=0.0
         wilt=.true.
        end if
       end if
      
c      Calculate daily percolation from layer 1 to 2

       perc = k(1)*w(1)**4.
c       evap = 0.10*deq(d)
       evap = 0.0
       
c      Exrati give rates of extraction from upper and lower soil layers

       if (wr.gt.0.0) then   
        r1(1) =      root  * (w(1)/wr)
        r1(2) =  (1.-root) * (w(2)/wr)       
       else
        r1(1) = 0.
        r1(2) = 0.
       end if  

c....................................................

c      Carry out daily water balance accounting!
       if (k(5).ne.0.0) then
        w(1)=w(1)+(dprec(d)+dmelt(d)-perc-evap-r1(1)*aet)/k(5)
       else
        w(1)=0.0
       end if

       if (k(6).ne.0.0) then
        w(2)=w(2)+(perc-r1(2)*aet)/k(6)
       else
        w(2)=0.0
       end if

c.....................................................

c      If w2 is filled beyond fc then get drainage from layer two
       drainage=0.

       if (w(2).ge.1.) then
        drainage = (w(2)-1.)*k(6)
        w(2) = 1.
       end if
       
c       if (w(2).ge.1.) then
c        drainage=k(1)/2.*w(2)**4.
c        w(2)=w(2)-drainage
c       end if

c      If w1 is filled above fc then get surface or sub-surface runoff
       runnoff=0.

       if(w(1).ge.1.)then
        runnoff = (w(1)-1.)*k(5)
        w(1) = 1.
       end if

c      To prevent errors, set moisture back to wp

       if (w(1).le.0.) w(1)=0. 
       if (w(2).le.0.) w(2)=0.

c-----------------------------------------------------------------------  
       end if                 !the cold temp sensitive loop ends here
c---------------------------------------------------------------------------

c      Sum the daily aet values:
       annaet = annaet + aet 

c      Sum the total runoff:
       sumoff=sumoff+runnoff+drainage

c      Sum the monthly total runoff:
       runoffmonth(month)=runoffmonth(month)+runnoff+drainage

c      Sum the daily fvc value and average the daily gc value
       meanwr(month,1) = meanwr(month,1) + wr   / days(month)
       meanwr(month,2) = meanwr(month,2) + w(1) / days(month)
       meanwr(month,3) = meanwr(month,3) + w(2) / days(month)

       if (gc.ne.0.0) then
        meangc(month) =  meangc(month) +   gc / days(month)
       end if
       if (fvc.ne.0.0) then
        meanfvc(month)= meanfvc(month) +  fvc / days(month)
       end if
       meanaet(month)= meanaet(month) +  aet / days(month)

       wet(d)=wr
       dayfvc(d)=fvc

 120   continue

 110   continue
 100   continue
    
       return
       end
c***************************************************************************
c      Calculate the a generic phenology for any summergreen pft
c      A three month period centred around the coldest month is
c      defined as the minimum period during which foliage is not
c      present. Plants then start growing leaves and the end of this 
c      3 month period or when the temperature gos above 5oC if this
c      occurs later. Plants take 200 gdd5 to grow a full leaf canopy:

       subroutine phenology
     >     (dphen,dtemp,temp,tcm,tdif,tmin,pft,ddayl,pftpar)

       implicit none

       integer day,spinup,m,dayofmonth,ncm,daysinmonth(12)
       integer coldm(3),phencase,winter,flip,pft,hotm

       real dphen(365,2),dtemp(365),ramp(2),tcm,gdd,temp(12),ont
       real today,tdif,tmin,ddayl(365),warm,pftpar(25,25)

       data (daysinmonth(m),m=1,12) 
     *   / 31,28,31,30,31,30,31,31,30,31,30,31 /

c-----------------------------------------------------------------------

       ramp(1)=pftpar(pft,8)   

       if(pft.eq.7)then
        ont=0.0                  !this sets the minimum temp for growth
       else
        ont=5.0
       endif


c      Set grass ramp value:
       ramp(2)=pftpar(pft,9)

c      Find the months with the warmest and coldest temperatures (cm):
       warm=tcm
       do m=1,12
        if(temp(m).eq.tcm) ncm=m
        if(temp(m).gt.warm) then
         warm=temp(m)
         hotm=m
        end if
       end do

       do 10 phencase=1,2

       coldm(2)=ncm
       coldm(1)=coldm(2)-1
       coldm(3)=coldm(2)+1

       if (coldm(1).eq.0) coldm(1)=12
       if (coldm(3).eq.13) coldm(3)=1
       if (hotm.eq.12) hotm=0

       gdd = 0.
       winter=0

       do 20 spinup=1,2
        day = 0

        do 30 m=1,12
         do 40 dayofmonth=1,daysinmonth(m)            
          day=day+1

          if (dtemp(day).gt.ont) then
          if (m.ne.coldm(1).and.m.ne.coldm(2).and.m.ne.coldm(3)) then
           today=(dtemp(day))
           if(today.le.0.) today=0.
           gdd = gdd + today
           if (gdd.eq.0.0) then
            dphen(day,phencase) = 0.0
           else
            dphen(day,phencase) = gdd/ramp(phencase)
           end if
           if (gdd.ge.ramp(phencase)) dphen(day,phencase)=1.
           flip=1
          else
           if (flip.eq.1) winter=0
           winter=winter+1
           dphen(day,phencase) = 0.
           gdd  = 0.
           flip = 0
          end if
          end if

c         remove deciduous leaves in the fall when the temp or 
c         photoperiod reaches a threshold.

          if (phencase.eq.1) then
           if (m.ge.hotm) then
            if (dtemp(day).lt.-10.0.or.ddayl(day).lt.10.0) then
             dphen(day,phencase)=0.
            end if
           else if (m.eq.coldm(1)) then
             dphen(day,phencase)=0.
           end if
          else if (phencase.eq.2) then
           if (dtemp(day).lt.-5.0) dphen(day,phencase)=0. 
          end if
       
 40      continue   !daily loop ends
 30     continue    !montlhy loop ends
 20    continue     !annual loop ends
 10    continue     !case (grass,tree) loop ends

 100   continue
       
       return
       end
c*************************************************************************
c      subroutine to calc GDDs, TCM, wrin, and total precipitation

       subroutine climdata(cold,warm,gdd5,gdd0,rain,
     *  temp,prec,dtemp,alttmin)
       implicit none
       integer m,day
       real cold,gdd5,warm,gdd0,rain
       real temp(12),prec(12),dtemp(365)
       real minus0,minus5,gdd10,minus10
       real alttmin

       cold = 100.
       warm = -100.
       rain = 0.

       do m = 1,12
        if(temp(m).lt.cold) cold=temp(m)
        if(temp(m).gt.warm) warm=temp(m)
        rain = rain + prec(m)
       end do

       gdd10= 0.
       gdd5 = 0.
       gdd0 = 0.

       do day = 1,365
        minus10= dtemp(day) - 10.
        minus5 = dtemp(day) - 5.
        minus0 = dtemp(day)
        if(minus10.le.0.) minus10 = 0.
        if(minus5.le.0.)  minus5  = 0.
        if(minus0.le.0.)  minus0  = 0.
        gdd10 = gdd10 + minus10
        gdd5  = gdd5  + minus5
        gdd0  = gdd0  + minus0
       end do

       alttmin=(0.006*cold**2)+(1.316*cold)-21.9

       return
       end
c--------------------------------------------------------------------------
c      subroutine provides PFT specific parameters stored within subroutine

       subroutine pftdata(pftpar)

       implicit none

       integer iv,ip,npft,npar

       real var(25,25),pftpar(25,25)

       parameter(npar=11,npft=13)

C      Define all PFT specific parameters
c          1  = Phenological type 1=evergreen,2=summergreen,3=raingreen
c          2  = maximal value for minimum canopy conductance
c          3  = value for Emax, maximum daily transpiration rate 
C          4  = value of sw below which raingreen leaves drop
C          5  = value of sw above which raingreen leaves appear
c          6  = fraction of roots in top soil layer, 30 cm from Jackson et al.
c          7  = Expected leaf longevity in months
c          8  = Number of GDD5 required for full leaf out 
c          9  = Number of GDD0 required for full leaf out
c         10  = presence of sapwood respiration
c         11  = c4 plant or not
*    LIST OF ALL PLANT TYPES
*    1 = tet Tropical Evergreen Trees   
*    2 = trt Tropical Drought-deciduous Trees (raingreens) 
*    3 = tbe Temperate Broadleaved Evergreen Trees 
*    4 = tst Temperate Deciduous Trees
*    5 = ctc Cool Conifer Trees
*    6 = bec Boreal Evergreen Trees     
*    7 = bst Boreal Deciduous Trees   
*    8 = C3/C4 temperate grass plant type
*    9 = C4 tropical grass plant type
*    10= C3/C4 woody desert plant type
*    11= Tundra shrub type
*    12= cold herbaceous type
*    13= Lichen/forb type

       data ((var(iv,ip),ip=1,npar),iv=1,npft)     /   
     *  1., 0.5, 10.0, -99.,-99. ,0.69 ,18. , -99.,-99., 1., 0., 
     *  3., 0.5, 10.0, 0.5, 0.6  ,0.70 , 9. , -99.,-99., 1., 0.,
     *  1., 0.2,  4.8, -99.,-99. ,0.67 ,18. , -99.,-99., 1., 0.,
     *  2., 0.8, 10.0, -99.,-99. ,0.65 , 7. , 200.,-99., 1., 0.,
     *  1., 0.2,  4.8, -99.,-99. ,0.52 ,30. , -99.,-99., 1., 0.,
     *  1., 0.5,  4.5, -99.,-99. ,0.83 ,24. , -99.,-99., 1., 0.,
     *  2., 0.8, 10.0, -99.,-99. ,0.83 ,24. , 200.,-99., 1., 0.,
     *  3., 0.8,  6.5,  0.2, 0.3 ,0.83 , 8. , -99.,100., 2., 1.,
     *  3., 0.8,  8.0,  0.2, 0.3 ,0.57 ,10. , -99.,-99., 2., 1.,
     *  1., 0.1,  1.0, -99.,-99. ,0.53 ,12. , -99.,-99., 1., 1.,
     *  1., 0.8,  1.0, -99.,-99. ,0.93 , 8. , -99.,-99., 1., 0.,
     *  2., 0.8,  1.0, -99.,-99. ,0.93 , 8. , -99., 25., 2., 0.,
     *  1., 0.8,  1.0, -99.,-99. ,0.93 , 8. , -99.,-99., 1., 0.   /

       do iv = 1,npft
         do ip = 1,npar
           pftpar(iv,ip) = var(iv,ip)
         end do
       end do

       return
       end
c**********************************************************************
c      Subroutine to get soil parameters given soil class number

       subroutine soildata(k,soil)
       implicit none
       integer i,j,soil(3),stype
       real k(12),whc,d1,d2,store(3,12)
       parameter(d1=300.,d2=1200.)
       data ((store(i,j),i=1,2),j=1,9) /
c-------------------------------------------------------------
c       FAO soil texture data set values: 
c       1 coarse, 2 medium, 3 fine
c       4 medium-coarse, 
c       5 fine-coarse, 
c       6 fine-medium, 
c       7 fine-medium-coarse,
c       8 Organic, 9 Ice
c       Selected FAO soil types: 
c       1 Kastozems,2 Chernozems,3 Vertisols, 4 Phaeozems
c
c       k1      whc  
     *  5.,        0.11,  
     *  4.,       0.15,   
     *  3.,       0.12,   
     *  4.5,       0.13,   
     *  4.,       0.115,  
     *  3.5,       0.135,   
     *  4.,       0.127,
c       Organic soils:
     *  9.,       0.30,  
c       Extra soil type:
     *  0.2,       0.10 /

c     *  5.,        0.11,  
c     *  4.,       0.15,   
c     *  3.,       0.12,
c     *  4.5,       0.13,   
c     *  4.,       0.115,  
c     *  3.5,       0.135,   
c     *  4.,       0.127,
c-------------------------------------------------------------       
c      Model should not be called for grid cells mapped as ice (soil=9)
       if(soil(2).ge.9) stop 'Value of soil type is not allowed!'

c      Assign the model soil type based on the FAO soil data base:
c      if zobler soil type is fine AND soil is a vertisol the define
c      heavy clay soil textural type:
       if(soil(3).eq.1.and.soil(2).eq.3)then
       stype=9
       else
       stype=soil(1)
       endif

c      Define water holding capacity for both soil layers (mm)
       whc  = store(2,stype) !real(soil(2))

       k(5) = whc*d1   !call this the top 30cm of a 1.5m deep soil
       k(6) = whc*d2   !and this the rest

c      Define k1 value (texture-dependent)
       k(1) = store(1,stype)
     
c      Define k2 value (not texture-dependent) 
       k(2) = 4.


       return
       end
c**********************************************************************
c     subroutine constraints provides environmental sieve

      subroutine constraints
     >      (tcm,twm,tminin,gdd5,rad0,pfts,tmin,maxdepth,gdd0)

      implicit none        

      integer npft,nclin,ip,iv,il
      integer pfts(13)

      parameter(npft=13,nclin=6)

      real limits(npft,nclin,2),clindex(nclin),undef
      real tmin,tcm,twm,ts,gdd5,rad0,gdd0,tminin,maxdepth

      parameter(undef=-99.9)

c------------------------------------------------------
*    LIST OF THE THIRTEEN PLANT FUNCTIONAL TYPES
*    1 = tet = Tropical Evergreen 
*    2 = trt = Tropical Raingreen
*    3 = wte = Temperate Broadleaved Evergreen   
*    4 = tst = Temperate Summergreen 
*    5 = ctc = Temperate Evergreen Conifer        
*    6 = bec = Boreal Evergreen       
*    7 = bst = Boreal Deciduous  
*    8 = temperate grass
*    9 = tropical/warm-temperate grass
*   10 = Desert woody plant type C3, C4
*   11 = Tundra shrub type
*   12 = Cold herbaceous type
*   13 = Lichen/forb type

c     Define and initialize the limits of the climatic indices 

      data(((limits(ip,iv,il),il=1,2),iv=1,5),ip=1,npft) /
     +-99.9,-99.9,   0.0,-99.9, -99.9,-99.9, -99.9,-99.9,  10.0,-99.9,   !1
     +-99.9,-99.9,   0.0,-99.9, -99.9,-99.9, -99.9,-99.9,  10.0,-99.9,   !2
     +-99.9,-99.9,  -8.0,  5.0,1200.0,-99.9, -99.9,-99.9,  10.0,-99.9,   !3
     +-15.0,-99.9, -99.9, -8.0,1200.0,-99.9, -99.9,-99.9, -99.9,-99.9,   !4
     + -2.0,-99.9, -99.9, 10.0, 900.0,-99.9, -99.9,-99.9,  10.0,-99.9,   !5
     +-32.5, -2.0, -99.9,-99.9, -99.9,-99.9, -99.9,-99.9, -99.9, 21.0,   !6
     +-99.9,  5.0, -99.9,-10.0, -99.9,-99.9, -99.9,-99.9, -99.9, 21.0,   !7
     +-99.9,-99.9, -99.9,  0.0, 550.0,-99.9, -99.9,-99.9, -99.9,-99.9,   !8
     +-99.9,-99.9,  -3.0,-99.9, -99.9,-99.9, -99.9,-99.9,  10.0,-99.9,   !9
     +-99.9,-99.9, -45.0,-99.9, 500.0,-99.9, -99.9,-99.9,  10.0,-99.9,   !10
     +-99.9,-99.9, -99.9,-99.9, -99.9,-99.9,  50.0,-99.9, -99.9, 15.0,   !11
     +-99.9,-99.9, -99.9,-99.9, -99.9,-99.9,  50.0,-99.9, -99.9, 15.0,   !12
     +-99.9,-99.9, -99.9,-99.9, -99.9,-99.9, -99.9,-99.9, -99.9, 15.0  / !13
c      ltcm, utcm,  lmin, umin,  lgdd, ugdd, lgdd0,ugdd0,  ltwm, utwm 

      data((limits(ip,6,il),il=1,2),ip=1,npft) /
     +-99.9,-99.9,  !1
     +-99.9,-99.9,  !2
     +-99.9,-99.9,  !3
     +-99.9,-99.9,  !4
     +-99.9,-99.9,  !5
     +-99.9,-99.9,  !6
     +-99.9,-99.9,  !7
     +-99.9,-99.9,  !8
     +-99.9,-99.9,  !9
     +-99.9,-99.9,  !10
     + 15.0,-99.9,  !11
     +-99.9,-99.9,  !12
     +-99.9,-99.9 / !13
c     lsnow,usnow

c     Assign tmin value
      if (tminin.le.tcm) then
       tmin=tminin
      else
       tmin=tcm-5.0
      end if

c     Assign ts value
      ts=twm-tcm

c     set up climate indices array:
      clindex(1)=tcm      !temperature of the coldest month
      clindex(2)=tmin     !absolute minimum temperature
      clindex(3)=gdd5     !GDDays above 5 deg C
      clindex(4)=gdd0     !gdd0
      clindex(5)=twm
      clindex(6)=maxdepth !snow depth

c Determines the values of the climatic indices are within the climatic limits 

      do 100 ip=1,npft
         do 101 iv=1,nclin
*
* both limits given, value inside - PRESENT:
*
            if(limits(ip,iv,1).le.clindex(iv).and.
     +         limits(ip,iv,1).ne.undef.and.
     +         limits(ip,iv,2).ne.undef.and.
     +         limits(ip,iv,2).gt.clindex(iv))then
               pfts(ip)=1
*
* or lower limit missing, value below upper level - PRESENT:
*
            elseif(limits(ip,iv,1).eq.undef.and.
     +             limits(ip,iv,2).ne.undef.and.
     +             limits(ip,iv,2).gt.clindex(iv))then
               pfts(ip)=1
*
* or upper limit missing, value above lower level - PRESENT:
*
            elseif(limits(ip,iv,1).le.clindex(iv).and.
     +             limits(ip,iv,1).ne.undef.and.
     +             limits(ip,iv,2).eq.undef)then
               pfts(ip)=1
*
* both limits missing - PRESENT:
*
            elseif(limits(ip,iv,1).eq.undef.and.
     +             limits(ip,iv,2).eq.undef)then
               pfts(ip)=1
*
* none of these - ABSENT:
*
            else
               pfts(ip)=0

               goto100
            endif
101         continue
100      continue

       return
       end
c******************************************************************************
c     subroutine snow masks precip to account for effects of snow

      subroutine snow(dtemp,dprec,dmelt,dprecin,maxdepth)
      implicit none
      integer day,it
      real dtemp(1:365),dprec(1:365),dprecin(1:365)
      real tsnow,km,snowpack,snowmelt,newsnow,drain
      real dmelt(365),sum1,sum2,maxdepth
      parameter(tsnow=-1.,km=0.7)
      
      snowpack = 0.0
      maxdepth = 0.0

      do it =1,2
       sum1=0.
       sum2=0.

       do day=1,365
   
        drain = dprecin(day) / (365./12.)

c       Calculate snow melt and new snow for today
        if(dtemp(day).lt.tsnow)then
         newsnow  = drain
         snowmelt = 0.
        else
         newsnow  = 0.
         snowmelt = km*(dtemp(day)-tsnow)
        endif

c       Reduce snowmelt if greater than total snow remaining
        if (snowmelt.gt.snowpack) snowmelt = snowpack
    
c       Update snowpack store
        snowpack = snowpack + newsnow - snowmelt
        if (snowpack.gt.maxdepth) maxdepth=snowpack

c       Calculate effective water supply (as daily values in mm/day)
        dprec(day) = drain - newsnow 
        dmelt(day) = snowmelt

        sum1=sum1+dprec(day)+dmelt(day)
        sum2=sum2+drain

       end do
      end do

      return
      end
c******************************************************************************
c      Calculates insolation and PET for each month
 
       subroutine  ppeett
     > (lat,dtemp,dclou,dpet,temp,sun,dayl,rad0,ddayl,radanom)

       implicit none

       integer month,day,dayofm,midday(12),daysinmonth(12)

       real dtemp(365),dclou(365),lat
       real dpet(365),ddayl(365),temp(12)
       real sun(12),dayl(12),rad0
       real dip,pie,a,sat,cla,sla,ho,rl,fd,qo,rs
       real psi,l,b,radup,qoo,c,d,albedo,hos,u,v,us,vs
       real radanom(12)

       parameter(b=0.2,radup=107.,qoo=1360.,d=0.5,c=0.25)
       parameter(albedo=0.17)

       data (midday(month),month=1,12) 
     *   / 16,44,75,105,136,166,197,228,258,289,319,350 /
       data (daysinmonth(month),month=1,12) 
     *   / 31,28,31,30,31,30,31,31,30,31,30,31          /

       pie = 4.*atan(1.)
       dip = pie/180.
    
c      Daily loop
       day=0
       rad0=0.
       do 10 month  = 1,12
       do 20 dayofm = 1,daysinmonth(month)
       day=day+1

c      Find psi and l for this temperature from lookup table
c      psychrometer constant (pa/oc), latent heat lamba (mj/kg) 
       call table(dtemp(day),psi,l)
    
c      Calculation of longwave radiation       
       rl = (b + (1-b)*(dclou(day)/100.))*(radup- dtemp(day))

c      Since changes in radiation (short or long) will mainly be due
c      to changes in cloudiness, apply the (short wave) anomaly here too.
c      Per B. Smith 1998
       
       rl=rl*radanom(month)

c      c=0.29*cos(lat) to emphasize the effect of clouds at high latitude
c      c=0.29*cos(lat*dip)

c      Calculation of short wave radiation
       qo  =  qoo*(1.+2.*0.01675*cos(dip*(360.*real(day))/365.))
       rs  =  qo*(c+d*(dclou(day)/100.))*(1.-albedo)

       rs=rs*radanom(month)

       a   = -dip*23.4*cos(dip*360.*(real(day)+10.)/365.)
       cla =  cos(lat*dip)*cos(a)
       sla =  sin(lat*dip)*sin(a)
       u = rs*sla - rl
       v = rs*cla      

c      Check for polar day and polar night
       if(u.ge.v)then
c      polar day:
       ho = pie
       elseif(u.le.(0.-v))then
c      polar night:
       ho = 0.
       else
c      normal day and night: (find ho the time of dawn)
       ho =  acos(-u/v)
       endif
     
c      Equations for demand function
       sat=(2.5*10**6.*exp((17.27*dtemp(day))/(237.3+dtemp(day))))
     *          /((237.3+dtemp(day))**2.)
c      Multiply l by e6 to convert from mj/kg to j/kg 
       fd = (3600./(l*1e6))*(sat/(sat+psi))

c      Store total daily equilibrium transpiration rate as dpet
       dpet(day)=fd*2.*((rs*sla-rl)*ho+rs*cla*sin(ho))/(pie/12.)

c      Calculate daylength in hours
       if (ho.eq.0.0) then
        ddayl(day)=0.0
       else
        ddayl(day) = 24.*(ho/pie)
       end if

c      If at a mid-month day then record mid-month daily sun and dayl
       if(day.eq.midday(month))then


c        First record the day length 
         dayl(month)=ddayl(day)

c        Now calculate daily total irradiance (j/m2) & record in sun
         us = rs*sla
         vs = rs*cla
c        check for polar day and polar night
         if(us.ge.vs)then
c        polar day:
         hos = pie
         elseif(us.le.(0.-vs))then
c        polar night (also h1=0. for polar night)
         hos = 0.
         else
c        normal day and night, find hos the time of dawn
         hos =  acos(-us/vs)
         endif

c        Find total insolation for this day, units are j/m2
         sun(month)= 
     *   2.*(rs*sla*hos+rs*cla*sin(hos))*(3600.*12./pie)
c        Do not allow negative values for insolation 
         if(sun(month).le.0.) sun(month)=0.

c        Sum total annual radiation for months with t>0oC (GJs PAR year-1)
c        (assuming 50% of short wave radiation is PAR)
         if(temp(month).gt.0.)then
         rad0=rad0+real(daysinmonth(month))*sun(month)*1e-9*0.5
         endif

       endif

 20    continue
 10    continue

       return
       end
***************************************************************************
C      subroutine table from bucket subroutine

      subroutine table(tc,gamma,lambda)

c looks up gamma and lambda from table (essential part of EVAPO.F)

c Author: Wolfgang Cramer, Dept. of Geography, Trondheim University-AVH,
c N-7055 Dragvoll, Norway.

c latest revisions 14/2-1991

c enable this when you run on a compiler allowing for it:
      implicit none

c on UNIX, please compile with "f77 -u"

      integer ir,il
      real gbase(2,11),lbase(2,11)
      real gamma,lambda,tc

      data ((gbase(ir,il),ir=1,2),il=1,11)
     > /-5.,64.6, 0.,64.9, 5.,65.2,10.,65.6,15.,65.9,20.,66.1,
     >  25.,66.5,30.,66.8,35.,67.2,40.,67.5,45.,67.8/
      data ((lbase(ir,il),ir=1,2),il=1,11)
     > /-5.,2.513, 0.,2.501, 5.,2.489,10.,2.477,15.,2.465,20.,2.454,
     >  25.,2.442,30.,2.430,35.,2.418,40.,2.406,45.,2.394/

c temperature above highest value - set highest gamma and lambda and return

      if(tc.gt.gbase(1,11)) then
         gamma=gbase(2,11)
         lambda=lbase(2,11)
         return
      endif

c temperature at or below value - set gamma and lambda

      do 100 il=1,11
         if(tc.le.gbase(1,il)) then
            gamma=gbase(2,il)
            lambda=lbase(2,il)
            return
         endif
100   continue

      end
***************************************************************************
      subroutine daily(mly,dly)
      implicit none
      real mly(12),dly(365),midday(12),vinc
      integer im,id
      data (midday(im),im=1,12)/16., 44., 75.,105.,136.,166.,
     >                         197.,228.,258.,289.,319.,350./

      vinc=(mly(1)-mly(12))/31.0
      dly(350)=mly(12)
      do 100 id=351,365
         dly(id)=dly(id-1)+vinc
100      continue
      dly(1)=dly(365)+vinc
      do 101 id=2,15
         dly(id)=dly(id-1)+vinc
101      continue
      do 103 im=1,11
         vinc=(mly(im+1)-mly(im))/(midday(im+1)-midday(im))
         dly(int(midday(im)))=mly(im)
         do 104 id=int(midday(im))+1,int(midday(im+1))-1
            dly(id)=dly(id-1)+vinc
104         continue
103      continue
      return
      end   
**************************************************************************
      subroutine isotope(Cratio,Ca,temp,Rd,c4month,mgpp,phi,
     >meanC3,meanC4,C3DA,C4DA,gpp)

c     This subroutine is for calculating the total fractionation of 13C
c     as it goes from free air (as 13CO2) to fixed carbon in the leaf.
c     For use with the BIOME3 model of A. Haxeltine (1996).
c     There are separate routines for calculating fractionation by both
c     C3 and C4 plants.  This program is based upon the model used by
c     Lloyd and Farquhar (1994).

      implicit none

      logical c4month(12)

      integer m

      real Cratio(12),Ca,temp(12),Rd(12),mgpp(12),gpp
      real C3DA(12),C4DA(12),meanC3,meanC4
      real wtC3,wtC4
      real delC3,delC4,phi

c      open (unit=1,file='del13C3.out',status='unknown')
c      open (unit=2,file='del13C4.out',status='unknown')

      wtC3=0.0
      wtC4=0.0

      do m=1,12
      if (mgpp(m).gt.0.0) then

       if (Cratio(m).lt.0.05) Cratio(m)=0.05

       if (c4month(m)) then
        call isoC4(Cratio(m),phi,temp(m),delC4)
        C3DA(m)=delC4
        wtC3 = wtC3+delC4*mgpp(m)
       else
        call isoC3(Cratio(m),Ca,temp(m),Rd(m),delC3)
        C3DA(m)=delC3
        wtC3 = wtC3+delC3*mgpp(m)
       end if
      else
       C3DA(m)=0.0
      end if
      end do
      
      meanC3 = wtC3/gpp
      meanC4 = wtC4/gpp
      
c      write(*,*)meanC3,C3DA(4)
c      write(2,10)meanC4,(C4DA(m),m=1,12)
c 10   format(F7.2,12F7.2)

      return
      end

c----------------------------------------------------------------
c     This part calculates fractionation for C3 photosynthesis.
      subroutine isoC3(Cratio,Ca,temp,Rd,delC3)
  
      implicit none
      real DeltaA,delC3
      real a,es,a1,b,e,k,f,gamma,Catm
      real Cratio,Ca,Rd,temp,leaftemp
      real q,r,s,t

c     define fractionation parameters

       a= 4.4
      es= 1.1
      a1= 0.7
       b=27.5
       e= 0.0
       f= 8.0
      Catm= 0.0 

      if (Rd.le.0) Rd=0.01

c      write(*,*)Ca,Cratio,Rd

      leaftemp = 1.05*(temp+2.5)
      gamma = 1.54*leaftemp
      Rd = Rd/(86400.0*12.0)
      Catm = Ca*1.0e6
      k = Rd/11.0            !From Farquhar et al. 1982 p. 126

c     calculate the fractionation

      q = a*(1-Cratio+0.025)
      r = 0.075*(es+a1)
      s = b*(Cratio-0.1)
      t = 0.0  !(e*Rd/k+f*gamma)/Catm

      DeltaA = q+r+s-t

      delC3 = DeltaA

c      write(*,*)delC3

      return
      end
c---------------------------------------------------------------
c     This part calculates fractionation for C4 photosynthesis.

      subroutine isoc4(Cratio,phi,temp,delC4)

      implicit none
      real DeltaA,delC4
      real a,es,a1,b4,b3,phi,temp
      real Cratio

         a= 4.4
        es= 1.1
        a1= 0.7
        b3=30.0
c       phi= 0.2

      b4=(26.19-(9483/(273.2+temp)))
 
      DeltaA=a*(1-(Cratio)+0.0125)+0.0375*(es+a1)+
     >       (b4+(b3-es-a1)*phi)*((Cratio)-0.05)

      delC4 = DeltaA
      
      return
      end
c--------------------------------------------------------------------
c     This subroutine is for calculating the phi variable used in 
c     C4 photosynethsis isotope fractionation calculations

      subroutine calcphi(gpp,phi)

      implicit none
      real gpp(12),totgpp,meangpp,normgpp(12)
      real snormavg(4),svar(4)
      real avar,phi,a
      integer m,s

      totgpp=0.0       !initialize a few variables
      do s=1,4
       svar(s)=0.0
      end do

c     This first part of the subroutine estimates annual variability of
c     GPP first by normalizing and then summing seasonal variability
c     which compensates for amplitude and seasonal variation in GPP.

      do m=1,12
       totgpp=totgpp+gpp(m)
      end do

      meangpp=totgpp/12.0

      do m=1,12
       normgpp(m)=gpp(m)/meangpp
      end do 

      snormavg(1)=(normgpp(1)+normgpp(2)+normgpp(3))/3.0
      snormavg(2)=(normgpp(4)+normgpp(5)+normgpp(6))/3.0
      snormavg(3)=(normgpp(7)+normgpp(8)+normgpp(9))/3.0
      snormavg(4)=(normgpp(10)+normgpp(11)+normgpp(12))/3.0

c     calculate the population variances by season

      do m=1,3
       a=((normgpp(m)-snormavg(1))**2)/3
       svar(1)=svar(1)+a
      end do

      do m=4,6
       a=((normgpp(m)-snormavg(2))**2)/3
       svar(2)=svar(2)+a
      end do

      do m=7,9
       a=((normgpp(m)-snormavg(3))**2)/3
       svar(3)=svar(3)+a
      end do

      do m=10,12
       a=((normgpp(m)-snormavg(4))**2)/3
       svar(4)=svar(4)+a
      end do

      avar=svar(1)+svar(2)+svar(3)+svar(4)
c------------------------------------------------

c     This part sets the phi value based upon the annual variability.
c     The equation is a simple regresion based upon hypothetical extreme
c     scenarios of phi.

      phi=0.3518717*avar+0.2552359

      if (phi.ge.1.0) phi=phi/10.0

      return

      end

c---------------------------------------------------------------------------
c
c     This subroutine models heterotrophic respiration of litter and soíl
c     organic carbon in both a fast and a slow pool.  It assumes equilibrium 
c     and so decays all of a given year's NPP.  The 13C composition of respired
c     CO2 is also modelled.  Models are based on the work of Foley, Lloyd and
c     Taylor, and Sitch.

      subroutine hetresp
     >(pft,nppann,tair,tsoil,aet,moist,Rlit,Rfst,Rslo,Rtot,
     >isoveg,isoR,isoflux,Rmean,meanKlit,meanKsoil)

      implicit none

      real tair(12),moist(12),tsoil(12)
      real Plit,Pfst,Pslo,Rtot(12),aet(12)
      real Klit(12),Kfst(12),Kslo(12)
      real Rlit(12),Rfst(12),Rslo(12),isoR(12),Rmean
      real klitsum,kfstsum,kslosum
      real Rten,mfact,nppann,isoveg,isoatm
      real isolit(12),isofst(12),isoslo(12),isoflux(12)
      integer m,pft
      
      real meanKlit,meanKsoil

      parameter(isoatm=-8.0)
     
c     P is pool sizes for partitioning, R is respired CO2

c     the soil temp subroutine must have been called by now 

c     partition annual npp into pools according to Foley strategy

      if (nppann.le.0.0) then
       do m=1,12
        Rlit(m)=0.0
        Rfst(m)=0.0
        Rslo(m)=0.0
        Rtot(m)=0.0
        isoR(m)=0.0
        isoflux(m)=0.0
       end do
       isoveg=0.0
       return

      else               !begin the real routine here

       if (pft.eq.1.or.pft.eq.2) then
         Plit=0.650*nppann
         Pfst=0.980*0.350*nppann
         Pslo=0.020*0.350*nppann
       else
         Plit=0.700*nppann
         Pfst=0.985*0.300*nppann
         Pslo=0.015*0.300*nppann
       end if

c     Calculate respiration for each pool with an R10 base resp.
c     Litter needs to decay according to a basic temp and moist function.
c     Soil decay can be calculated according to temp. response of
c     Lloyd and moisture of Foley with a turnover time built into the Rten

c     Two ways to decay NPP, one based on surface temp and AET for litter 
c     (Foley).  The other is for soil decay and is based on soil 
c     temperature and moisture.

      Rten=1.0
      Klitsum=0.0
      Kfstsum=0.0
      Kslosum=0.0

      do m=1,12
       mfact=0.25+0.75*moist(m)

       Klit(m)=1.0*10.0**(-1.4553+0.0014175*aet(m))
       Klitsum=Klitsum+Klit(m)

       Kfst(m)=mfact*Rten*
     >  EXP(308.56*((1/56.02)-(1/(tsoil(m)+273.-227.13))))
       Kfstsum=Kfstsum+kfst(m)

       Kslo(m)=mfact*Rten*
     >  EXP(308.56*((1/56.02)-(1/(tsoil(m)+273.-227.13)))) 
       Kslosum=Kslosum+Kslo(m)
      end do

c-----added 23.07.99---------------

      meanKlit=Klitsum/12.
      meanKsoil=Kfstsum/12.

c----------------------------------

      Rmean=0

      do m=1,12
       Rlit(m)=Plit*(Klit(m)/Klitsum)
       Rfst(m)=Pfst*(Kfst(m)/Kfstsum)
       Rslo(m)=Pslo*(Kslo(m)/Kslosum)
       Rtot(m)=Rlit(m)+Rfst(m)+Rslo(m)
       Rmean=Rmean+(Rtot(m)/12.)
      end do

c     calculate the isotope ratio of respired CO2 based on
c     the NPP weighted mean 13C in the vegetation
c     Since 13C is enriched in organic matter over time add factors

      do m=1,12
       isolit(m)=isoveg-0.75  !these factors represent enrichment
       isofst(m)=isoveg-1.5
       isoslo(m)=isoveg-2.25
       isoR(m)=((Plit/nppann)*isolit(m))+
     > ((Pfst/nppann)*isofst(m))+((Pslo/nppann)*isoslo(m))
       isoflux(m)=(isoatm-isoR(m))*Rtot(m)
      end do

      end if
      return

      end 

c-----------------------------------------------------------------
c     This subroutine calculates monthly mean soil temperature 
c     based on monthly mean air temperature assuming a thermal
c     conductivity of the soil and a time lag between soil and air 
c     temperatures. Based on work by S. Sitch.

      subroutine soiltemp(tair,soiltext,tsoil)

      implicit none

      real tair(12),tsoil(12)
      real diffus,damp,amp,lag,pie
      real therm(9),soiltext(4)
      real sumtemp,meantemp
      integer m,i

      pie = 4.*ATAN(1.)    

      data (therm(i),i=1,9)  
     >/ 8.0,4.5,1.0,5.25,4.5,2.75,1.0,1.0,8.0 / !check value for soil 8

      sumtemp=0.
c----------------------------------
c     calculate a soil-texture based thermal cond. and lag time

      diffus = therm(2)
      damp=0.25/(sqrt(diffus))
      lag=damp*(6/pie)
      amp=exp(-damp)

c     calculate mean annual air temperature

      do m=1,12
       sumtemp=sumtemp+tair(m)
      end do
      meantemp=sumtemp/12.
      
c     calculate soil temperature
 
      tsoil(1) = (1.-amp)*meantemp+amp*(tair(12)+
     >(1.-lag)*(tair(1)-tair(12)))
     
      do m=2,12
       tsoil(m) = (1.-amp)*meantemp+amp*(tair(m-1)+
     > (1.-lag)*(tair(m)-tair(m-1)))
      end do

c     due to snow cover don't allow soil temp < -10

      do m=1,12
       if (tsoil(m).lt.-10.) tsoil(m)=-10.
      end do

      return

      end

c---------------------------------------------------------------------
      subroutine fire (wet,pft,lai,npp,firedays)

c     This subroutine calculates the number of potential fire days
c     in a year based on threshold values for soil moisture, which are
c     PFT dependent.  Jed Kaplan 1998.

c     May 1998 now includes a parameter to scale firedays in terms of
c     annual NPP so that firedays are reduced linearly to 0 below 1000gC/m2

      implicit none

      real wet(365),threshold(13),firedays,burn(365)
      real wetday,dryday,lai,npp,litter
      real firefraction,burnfraction

      integer day,pft,i

      data (threshold(i),i=1,13)
     >/0.25,0.20, 0.40,0.33,0.40, 0.33,0.33, 0.40,0.40, 0.33,
     >    0.33,0.33,0.33/

      firedays =   0.0
      wetday   =   0.0
      dryday   = 100.0

      do day=1,365
    
       if (wet(day).lt.threshold(pft)) then
        burn(day)=1.0
       else if (wet(day).gt.threshold(pft)+0.05) then
        burn(day)=0.0
       else
        burn(day)=1/exp(wet(day)-threshold(pft))
       end if

       if (wet(day).gt.wetday) wetday=wet(day)
       if (wet(day).lt.dryday) dryday=wet(day)

       firedays=firedays+burn(day)

      end do

      firefraction=firedays/365.0

      litter=(lai/5.0)*npp

      burnfraction=litter*(1.-(EXP(-0.2*firefraction**1.5))**1.5)

      if (npp.lt.1000.0) then
       firedays=firedays*(npp/1000.0)
      end if

      return

      end

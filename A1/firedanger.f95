    program fire

!   Variable declarations
    real dryBulb, wetBulb, precip, wind, buo
    real dryFactor, ffm, adfm, grass, timber, fload
    integer isnow, iherb
!   Input variables
    call userInput(dryBulb,wetBulb,precip,wind,buo,isnow,iherb)

!  Danger subroutine where the calculations is done
    call danger(dryBulb,wetBulb,isnow,precip,wind,buo,iherb,dryFactor,ffm,adfm,grass,timber,fload)

!   output of calculated info from danger
    call output(ffm, adfm, grass, timber, fload, buo)

    end

!  Input subroutine where the user inputs the values for each catergory.
    subroutine userInput(dryBulb,wetBulb,precip,wind,buo,isnow,iherb)
    write (*,*) 'INPUT: '
    write (*,*) ' Dry bulb temperature (Degrees Farenheit) = '
    read (*,*) dryBulb
    write (*,*) ' Dry bulb temperature = ', dryBulb
    write (*,*) ' Wet bulb temperature (Degrees Farenheit) = '
    read (*,*) wetBulb
    write (*,*) ' Wet bulb temperature = ', wetBulb
    write (*,*) ' Is there snow? (1 = Yes, 0 = No) = '
    read (*,*) isnow
    write (*,*) ' Is there snow? = ', isnow
    write (*,*) ' Wind speed (mph)? = '
    read (*,*) wind
    write (*,*) ' Wind speed mph? = ', wind
    write (*,*) ' Build up index (inches) = '
    read (*,*) buo
    write (*,*) ' Build up index = ', buo
    write (*,*) ' Herb state = '
    read (*,*) iherb
    write (*,*) ' Herb state = ', iherb
    write (*,*) ' Precipitation (inches)= '
    read (*,*) precip
    write (*,*) ' Precipitation = ', precip

    end

!  The output after danger subroutine calculations is printed
    subroutine output(ffm, adfm, grass, timber, fload, buo)
    write (*,*) 'OUTPUT: '
    write (*,*) ' Fine Fuel Moisture = ', ffm
    write (*,*) ' Adjusted Fuel Moisture = ', adfm
    write (*,*) ' Fine Fuel Spread = ', grass
    write (*,*) ' Timber Spread Index = ', timber
    write (*,*) ' Fire Load Index = ', fload
    write (*,*) ' Build Up Index = ', buo
    end

!  This subroutine is where majority of the calculations occur to find the outputs from the inputs given by the user.
    subroutine danger(dryBulb,wetBulb,isnow,precip,wind,buo,iherb,dryFactor,ffm,adfm,grass,timber,fload)
!   Initializing arrays and adding values to the arrays and variables
    real, dimension(4) :: a, b
    real, dimension(3) :: c
    real, dimension(6) :: d
    ffm = 99.
    adfm = 99.
    dryFactor = 0.
    fload = 0.
!   These are the table values used in computing the danger ratings
    a = (/-0.185900, -0.85900, -0.059660, -0.077373/)
    b = (/30.0, 19.2, 13.8, 22.5/)
    c = (/4.5, 12.5, 27.5/)
    d = (/16.0, 10.0, 7.0, 5.0, 4.0, 3.0/)
!   Test to see if there is snow on the ground
   if(isnow > 0) then
      grass=0.
      timber=0.
      if ( precip - .1 > 0) then
         buo=-50.*alog(1.-(1.-exp(-buo/50.))*exp( -1.175*(precip-.1)))
         if ( buo < 0) then
            buo = 0
         else
            return
         end if
      else
         return
      end if
! if there is no snow on the ground, compute the spread indexes and fire load.
   else
      dif=dryBulb-wetBulb
      do i=1,3
         if( dif-c(i) <= 0) then
            ffm=b(i)*exp(a(i)*dif)
            exit
         end if
      end do
   end if

!  Now it is computing the drying factor for the day.
   i=4
   do i=1,6
      if (ffm-d(i) > 0) then
         dryFactor=i-1
         exit
         
      else
         dryFactor = 7
      end if
   end do
   
!  We are now seeing if the fine fuel moisture is one or less, if it is, set fine fuel moisture to one.
   if (ffm-1. < 0) then
      ffm=1.

!  if fine fuel moisture is greater than one, than add five percent of FFM for each herb stage.
   else
      ffm = ffm + ( iherb-1 ) * 5.
      ! adjust BUO for precipatation before adding the drying factor.
      if (precip -.1 > 0) then
         buo=-50.*alog(1.-(1.-exp(-buo/50.))*exp(-1.175*(precip-.1)))
         ! add current drying factor if BUO is greater than and equal to zero, or else BUO is zero.
         if (buo < 0) then
            buo=0.0
         else
            buo=buo+dryFactor
            adfm = .9*ffm +.5 +9.5*exp(-buo/50.)
            ! adjusted fuel moisture is computed and is tested to see if moisture is over thrity percent.
            if ( adfm-30. < 0) then
               ! test to see if wind is greater than 14 percent.
               if ( wind-14. < 0) then
                  ! if it isnt over 14 percent, the following is calculated.
                  timber = .01312*(wind+6.) * (33.-adfm)**1.65 - 3.
                  grass = .01312*(wind+6.) * (33.-ffm)**1.65 - 3.
                  fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                  if ( fload > 0) then
                     fload = 10. ** fload
                     return
                  else
                     fload = 0.
                     return
                  end if
               
               else
                  ! if it is over 14 percent, the following is calculated.
                  timber = .00918*(wind+14.) * (33.-adfm)**1.65 - 3.
                  grass  = .00918*(wind+14.) * (33.-ffm)**1.65 - 3.
                  fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                  if ( fload > 0) then
                     fload = 10. ** fload
                     return
                  else
                     fload = 0.
                     return
                  end if
               end if
            
            ! check to see if FFM is greater than thirty percent, if it is, grass and timber are 1.
            else
               if ( ffm-30. < 0) then
                  timber = 1.
                  ! check to see if wind is greater than 14 mph. Depending on it, the following charateristics is calculated.
                  if ( wind-14. < 0) then
                     grass = .01312*(wind+6.) * (33.-ffm)**1.65 - 3.
                     if ( timber-1. > 0) then

                        if ( timber > 0) then

                           if ( buo > 0) then
                              fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                              ! fireload is calculated depending on timber and buo indexes
                              if ( fload > 0) then
                                 fload = 10. ** fload
                                 return
                              else
                                 fload = 0.
                                 return
                              end if

                           else
                              return
                           end if

                        else
                           return
                        end if
                 
                     ! check to see if wind is not greater than 14 mph. Depending on it, the following charateristics is calculated.
                     else
                        timber = 1.
                        if ( grass-1. < 0) then
                           grass = 1.

                           if ( timber > 0) then
                              
                              if ( buo > 0) then
                                 fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                                 ! fireload is calculated depending on timber and buo indexes
                                 if ( fload > 0) then
                                    fload = 10. ** fload
                                    return
                                 else
                                    fload = 0.
                                    return
                                 end if

                              else
                                 return
                              end if

                           else
                              return
                           end if

                        else
                           if ( timber > 0) then

                              if ( buo > 0) then
                                 fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                                 ! fireload is calculated depending on timber and buo indexes
                                 if ( fload > 0) then
                                    fload = 10. ** fload
                                    return
                                 else
                                    fload = 0.
                                    return
                                 end if

                              else
                                 return
                              end if

                           else
                              return
                           end if

                        end if

                     end if

                  ! if wind is not greater than 14 percent.
                  else
                     ! A different formula is used if wind is not greater than 14 percent.
                     grass  = .00918*(wind+14.) * (33.-ffm)**1.65 - 3.
                     if ( grass-99. > 0) then
                        grass = 99.
                        if ( timber-99. > 0) then
                           timber = 99.
                        else
                           if ( timber > 0) then

                              if ( buo > 0) then
                                 fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                                 ! fireload is calculated depending on timber and buo indexes
                                 if ( fload > 0) then
                                    fload = 10. ** fload
                                    return
                                 else
                                    fload = 0.
                                    return
                                 end if

                              else
                                 return
                              end if

                           else
                              return
                           end if

                        end if

                     else
                        if ( timber  > 0) then

                           if ( buo > 0) then
                              fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                              ! fireload is calculated depending on timber and buo indexes
                              if ( fload > 0) then
                                 fload = 10. ** fload
                                 return
                              else
                                 fload = 0.
                                 return
                              end if

                           else
                              return
                           end if

                        else
                           return
                        end if

                     end if

                  end if
               
               ! Outcome of fine fuel moisture being less than 30 percent.
               else
                  grass = 1.
                  timber = 1.
                  return
               end if

            end if

         end if
      
      ! if precipation factors no longer significant.
      else
         buo=buo+dryFactor
         adfm = .9*ffm +.5 +9.5*exp(-buo/50.)
         ! adjusted fuel moisture is computed and is tested to see if moisture is over thrity percent.
         if ( adfm-30. < 0) then
            ! test to see if wind is greater than 14 percent.
            if ( wind-14. < 0) then
               timber = .01312*(wind+6.) * (33.-adfm)**1.65 - 3.
            else
               timber = .00918*(wind+14.) * (33.-adfm)**1.65 - 3.
            end if

         ! if wind is not greater than 14 percent
         else
            if ( ffm-30. < 0) then
               timber = 1.
               
               if ( wind-14. < 0) then
                  grass = .01312*(wind+6.) * (33.-ffm)**1.65 - 3.

                  if ( timber-1. > 0) then

                     if ( timber > 0) then

                        if ( buo > 0) then
                           fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                           ! fireload is calculated depending on timber and buo indexes
                           if ( fload > 0) then
                              fload = 10. ** fload
                              return
                           else
                              fload = 0.
                              return
                           end if

                        else
                           return
                        end if

                     else
                        return
                     end if

                  else
                     timber = 1.
                     if ( grass-1. < 0) then
                        grass = 1.

                        if ( timber > 0) then

                           if ( buo > 0) then
                              fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                              ! fireload is calculated depending on timber and buo indexes
                              if ( fload > 0) then
                                 fload = 10. ** fload
                                 return
                              else
                                 fload = 0.
                                 return
                              end if

                           else
                              return
                           end if

                        else
                           return
                        end if

                     else
                        if ( timber > 0) then

                           if ( buo > 0) then
                              fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                              ! fireload is calculated depending on timber and buo indexes
                              if ( fload > 0) then
                                 fload = 10. ** fload
                                 return
                              else
                                 fload = 0.
                                 return
                              end if

                           else
                              return
                           end if

                        else
                           return
                        end if

                     end if

                  end if

               else
                  ! A different formula is used if wind is not greater than 14 percent.
                  grass  = .00918*(wind+14.) * (33.-ffm)**1.65 - 3.
                  if ( grass-99. > 0) then
                     grass = 99.
                     if ( timber-99. > 0) then
                        timber = 99.
                     else
                        if ( timber > 0) then

                           if ( buo > 0) then
                              fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                              ! fireload is calculated depending on timber and buo indexes
                              if ( fload > 0) then
                                 fload = 10. ** fload
                                 return
                              else
                                 fload = 0.
                                 return
                              end if

                           else
                              return
                           end if

                        else
                           return
                        end if

                     end if

                  else
                     if ( timber > 0) then

                        if ( buo > 0) then

                           fload=1.75*alog10( timber ) + .32*alog10( buo ) - 1.640
                           ! fireload is calculated depending on timber and buo indexes
                           if ( fload > 0) then
                                 fload = 10. ** fload
                                 return
                              else
                                 fload = 0.
                                 return
                              end if

                        else
                           return
                        end if

                     else
                        return
                     end if

                  end if

               end if

            ! Outcome of fine fuel moisture being less than 30 percent.
            else
               grass = 1.
               timber = 1.
               return
            end if
         end if
      end if
   end if
      
      end

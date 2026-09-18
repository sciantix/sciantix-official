function en = GB5DOF(P,Q,AlCuParameter,eRGB)
%  energy    computes the energy of an arbitrary boundary in FCC metals. 
%
%     en = GB5DOF(P,Q,AlCuParameter) computes the energy of a boundary
%     described by two rotation matrices P and Q. The character string 
%     variable AlCuParameter can take on one of four values: 'Al', 'Ni', 
%     'Au', or 'Cu'.  The output variable en is the computed boundary energy 
%     in units J/m^2. 
% 
%     en = GB5DOF(P,Q,AlCuParameter,eRGB) calculates the energy of a boundary 
%     in a hypothetical FCC metal defined by the numerical values of input 
%     parameters AlCuParameter and eRGB.  

%     eRGB defines the scale of boundary 
%     energy variations in the hypothetical metal and should be given in 
%     units J/m^2. When eRGB is defined, parameter AlCuParameter should 
%     take on a numerical value ranging from 0.0 (for aluminum) to 1.0 
%     (for copper). 
% 
%     P and Q are two properly normalized 3x3 rotation matrices defining 
%     orientations of the two grains with respect to a laboratory (sample) 
%     frame. For any vector V expressed in the cube frame of grain P (or Q), 
%     P*V (or Q*V) expresses the same vector in the laboratory frame. By 
%     convention, the first row of P (or Q) is the row vector of the boundary 
%     plane normal Np = P(1,:) (or Nq = Q(1,:)) written in the cube frame 
%     of grain P (or Q). Thus, P*Np' = Q*Nq' = [1 0 0]'. 
%
%     Examples
%
%       With P and Q matrices defined as follows
%
%           P = [ 0.5774    0.5774    0.5774 ;
%                 0.7071   -0.7071         0 ;
%                 0.4082    0.4082   -0.8165 ] 
%       
%           Q = [ 0.5774    0.5774    0.5774 ;
%               [-0.7071    0.7071         0 ;
%                -0.4082   -0.4082    0.8165 ]
%
%       en = GB5DOF(P,Q,'Ni') returns en = 0.0624 which is the energy in
%       metal Ni of the coherent twin boundary defined by matrices P and Q.
%
%       With the same matrices P and Q, en = GB5DOF(P,Q,0.768,1.445) returns 
%       the same value en = 0.0624. In this example numerical parameters
%       AlCuParameter = 0.768 and eRGB = 1.445 have values exactly matching
%       the best fit values of the same parameters for FCC metal Ni. 
      

    geom100 = distances_to_set(P,Q,'100'); % Generate geometry parameters
    geom110 = distances_to_set(P,Q,'110');
    geom111 = distances_to_set(P,Q,'111');

    if ~exist('eRGB','var') || isempty(eRGB),
        parvec = makeparvec(AlCuParameter);   % Option 2
    else
        parvec = makeparvec(AlCuParameter,eRGB);    % Option 1
    end

    en = weightedmeanenergy(geom100,geom110,geom111,parvec);    % Calculate the energy
end


function geom = distances_to_set(P,Q,whichaxes,dismax)
% geom = distances_to_set(P,Q,whichaxes,dismax)
%
% Calculates the geometry parameters for a given grain boundary relative to
% a given set of axes.
%
% P and Q are rotation matrices giving the orientations of the two grains.
% The grain boundary normal is fixed at [1,0,0].
%
% whichaxes is one of '100', '110', or '111'
%
% dismax is an optional parameter specifying the maximum distance to
% include. It defaults to slightly less than 1, which is the largest
% allowable distance before some anomalies start to appear.
%
% Result geom is a 4xn matrix, where the rows are distance, ksi, eta, and
% phi. It keeps all of the hits up to a distance of dismax.
%
% distance is 2*sin(delta/2) where delta is the angle of closest approach
% between a misorientation axis and one of the axes.  Note there are 24
% representations of the rotations and 3, 6, or 4 equivalent high-symmetry
% axes, so it calculates as many as 144 distances.  But only ones below
% the cutoff dismax are kept.
%
% Once it's picked the closest approximation to the boundary for a given
% axis and coset element, it finds the parameters ksi, eta, phi defining
% that idealized boundary (since the axis is defined, it's a 3-space).
%
% These are:
% phi, the angle between the rotation axis and the boundary plane normal
% (taken as the mean of the normals represented in the two actual grain
% orientations, which works when dismax is less than 1)
%
% ksi, the misorientation angle
%
% eta, a parameter giving the second axis of the boundary plane normal in
% terms of specified directions ('dirs') perpendicular to each
% high-symmetry axis.


    if ~exist('dismax','var') || isempty(dismax),
        dismax = 0.999999 ;  % Force the distance to be strictly less than one, allowing for roundoff
    end % Note if dismax >= 1, you're likely to get a warning about m1 being singular.

    switch whichaxes
        case {'110'}
            % Define 110 axes, normalize
            axes = [1  1  1  1  0  0 ;
                    1 -1  0  0  1  1 ;
                    0  0  1 -1  1 -1 ]/sqrt(2) ;

            % Define a crystal direction perpendicular to each rotation axis.
            % The formalism demands that this be an axis of at least two-fold
            % symmetry.
            dirs = [0  0  0  0  1  1 ;
                    0  0  1  1  0  0 ;
                    1  1  0  0  0  0 ] ;
        case {'111'}

            % Define 111 axes, normalize
            axes = [1  1 -1 -1 ;
                    1 -1  1 -1 ;
                    1 -1 -1  1 ]/sqrt(3) ;

            dirs = [1  1  1  1 ;
                   -1  1  1 -1 ;
                    0  0  0  0 ]/sqrt(2) ;

        case {'100'}
            % Define 100 axes, normalize
            axes = [1  0  0 ;
                    0  1  0 ;
                    0  0  1 ] ;

            dirs = [0  0  1 ;
                    1  0  0 ;
                    0  1  0 ] ;
        otherwise
            error('Undefined axis set')
    end

    naxes  = size(axes,2) ;
    period = pi*naxes/6 ;

    %  Define the symmetry operators

    rotX90  = [ 1  0  0 ;     %  Rotation by +90 degrees around X axis
                0  0 -1 ;
                0  1  0 ] ;

    rotY90  = [ 0  0  1 ;     %  Rotation by +90 degrees around Y axis 
                0  1  0 ;
               -1  0  0 ] ;   

    rotZ90  = [ 0 -1  0 ;      %  Rotation by +90 degrees around Z axis
                1  0  0 ;
                0  0  1 ] ;

    rotZ90m = [ 0  1  0 ;      %  Rotation by -90 degrees around Z axis
               -1  0  0 ;
                0  0  1 ] ;

    % Create 24 symmetry equivalent variants of Q
    % This is the coset appropriate for the rotation convention where Q'*P
    % is the misorientation represented in the grain frame.  If you're
    % getting odd results, e.g. misorientations that you know are CSL are
    % coming out entirely wrong, you may be using the opposite convention;
    % try replacing P and Q with P' and Q'.
    V = cell(24,1);

    V{1}  = Q;    
    V{2}  = V{1}*rotX90 ;        % Rotate the vectors three times around X by +90 degrees
    V{3}  = V{2}*rotX90 ;
    V{4}  = V{3}*rotX90 ;

    for j = 1:12                   % Rotate three times around Y by +90 degrees
      V{j+4} = V{j}*rotY90 ;
    end

    for j = 1:4
      V{j+16} = V{j} *rotZ90;    % Rotate three times around Z by +90 degrees
      V{j+20} = V{j} *rotZ90m;    % Rotate three times around Z by -90 degrees   
    end


    % Preallocate all parameter lists at their maximum possible sizes.
    % Redundant representations will be removed at the end.
    distances = zeros(1,24*naxes);
    phis      = zeros(1,24*naxes);
    ksis      = zeros(1,24*naxes);
    etas      = zeros(1,24*naxes);

    thisindex = 0;  % Number of hits found so far

    % Step through all combinations of symmetrically-equivalent axes and coset
    % elements V{j}.
    for i = 1:naxes

        ax   = axes(:,i) ;    % ax is a high-symmetry axis   

        dir = dirs(:,i) ;       %  This is the pivot vector used to partition 
                                %  the rotation around axis "i"
        dir2 = cross(ax,dir);   %  Completing the orthonormal coordinate set.
                                %  theta1 and theta2 are defined in the plane
                                %  spanned by (dir,dir2).


        for j = 1:24    % For each symmetry-related variant of the second grain

            Q     = V{j} ; 
            R     = Q'*P  ;  %  This rotates any vector in cube P into a vector in cube Q

            q = mat2quat(R);    % Calculation from here on out is much easier with quaternions.
            axi = q(2:4)'/sqrt(sum(q(2:4).^2)); % Normalized rotation axis
            psi = 2*acos(q(1)); % Rotation angle

            dotp  = axi*ax ;

            % Compute rotational distance from boundary P/Q to the rotation set "i" 
            % This formula produces 2*sin(delta/2), where delta is the angle of
            % closest approach.
            dis   = 2*sqrt(abs(1 - dotp*dotp))*sin(psi/2) ;

            if dis < dismax
                thisindex = thisindex + 1;

                theta = 2*atan(dotp*tan(psi/2)) ; % angle of rotation about ax that most closely approximates R

                % Compute the normal of the best-fitting GB in grain 1
                n1    = P(1,:)' ;
                n2    = Q(1,:)' ;

                RA = quat2mat([cos(theta/2);sin(theta/2)*ax]);
                % RA is the rotation about ax that most closely approximates R

                % From this point on we're dealing with the idealized rotation RA, not
                % the original rotation R.
                m1    = n1 + RA'*n2 ;

                % The next problem comes up only for very large distances,
                % which are normally cut off
                if norm(m1) < 0.000001
                    disp('m1 is singular!!!')
                end

                m1    = m1/norm(m1) ;   % Halfway between the two normal vectors from the two grains
                m2    = RA*m1 ;   % And the same represented in the other grain

                % Compute the inclination angle for the common rotation axis
                phi   = real(acos(abs(m1'*ax))) ; % "real" because of numerical problems when they're exactly parallel

                % Partition the total rotation angle "theta"
                if abs(ax'*m1) > 0.9999      % Check if the best-fitting GB is pure twist
                    theta1 = - theta/2 ;    % eta is meaningless for a twist boundary.
                    theta2 =   theta/2 ;
                else

                    theta1 = atan2(dir2'*m1,dir'*m1);
                    theta2 = atan2(dir2'*m2,dir'*m2);
                    % It's projecting m1 and m2 into the plane normal to ax and
                    % then determining the rotation angles of them relative to
                    % dir.

                end        

                % Reduce both angles to interval (-period/2,period/2],
                % semi-open with a small numerical error.
                theta2  = theta2 - round(theta2/period)*period ;
                theta1  = theta1 - round(theta1/period)*period ;

                % This implements the semi-open interval in order to avoid an
                % annoying numerical problem where certain representations are
                % double-counted.
                if abs(theta2+period/2)<0.000001,
                    theta2 = theta2 + period;
                end
                if abs(theta1+period/2)<0.000001,
                    theta1 = theta1 + period;
                end

                % Since this is only being run on fcc elements, which are
                % centrosymmetric, and all dir vectors are 2-fold axes, then
                % the operations of swapping theta1 and theta2, and of
                % multilying both by -1, are symmetries for the energy
                % function. This lets us fold everything into a small right
                % triangle in (ksi,eta) space:
                ksi     = abs(theta2 - theta1) ;
                eta     = abs(theta2 + theta1) ;

                % And store them in the vectors
                distances(thisindex) = dis;
                ksis(thisindex)      = ksi;
                etas(thisindex)      = eta;
                phis(thisindex)      = phi;
            end
        end      
    end   

    % Dump the excess pre-allocated ones and sort the rest in order of distance
    [distances,sortindex] = sort(distances(1:thisindex));
    ksis = ksis(sortindex);
    etas = etas(sortindex);
    phis = phis(sortindex);

    % Clean up redundancy.  Double-counting the same representation of one
    % boundary messes up the weighting functions in weightedmeanenergy.m

    % First round everything to 1e-6, so that negligible numerical
    % differences are dropped
    distances = 1e-6*round(distances*1e6);
    ksis = 1e-6*round(ksis*1e6);
    etas = 1e-6*round(etas*1e6);
    phis = 1e-6*round(phis*1e6);
    disp(ksis)
    % And finally create the 4 x thisindex array of geometrical parameters
    geom = unique([distances',ksis',etas',phis'],'rows')';

end


function q = mat2quat(m)
% q = mat2quat(m)
%
% Auxiliary function converts a rotation matrix, assumed orthonormal, into
% a unit quaternion.
    t = m(1,1)+m(2,2)+m(3,3);
    e0 = sqrt(1+t)/2;
    if t > -0.999999999
        e = [m(2,3)-m(3,2);m(3,1)-m(1,3);m(1,2)-m(2,1)]/(4*e0);
    else
        e0 = 0;
        e3 = sqrt(-(m(1,1)+m(2,2))/2);
        if abs(e3) > 2e-8   % Check for singularity, allowing numerical error
            e = [m(1,3)/(2*e3) ; m(2,3)/(2*e3) ; e3];
        else
            e1 = sqrt((m(1,1)+1)/2);
            if e1 ~= 0
                e = [e1;m(2,1)/(2*e1);0];
            else
                e = [0;1;0];
            end
        end
    end
    
    q = [e0;-e];
end


function m = quat2mat(q)
% m = quat2mat(q)
%
% Auxiliary function converts a quaternion into a rotation matrix with no
% assumption about normalization.
    e0 = q(1);
    e1 = q(2);
    e2 = q(3);
    e3 = q(4);

    m = [e0^2+e1^2-e2^2-e3^2 , 2*(e1*e2-e0*e3) , 2*(e1*e3+e0*e2); ...
          2*(e1*e2+e0*e3) , e0^2-e1^2+e2^2-e3^2 , 2*(e2*e3-e0*e1); ...
          2*(e1*e3-e0*e2) , 2*(e2*e3+e0*e1) , e0^2-e1^2-e2^2+e3^2 ]...
          /(e0^2+e1^2+e2^2+e3^2);
end



function [par43,AlCuparameter] = makeparvec(AlCuparameter,eRGB,par42Al,par42Cu)
% [par43,AlCuparameter] = makeparvec(AlCuparameter,eRGB,par42Al,par42Cu)
%
% Creates a 43-parameter vector as used by weightedmeanenergy
%
% Arguments are:
% AlCuparameter:  Position on the Al-Cu axis, where 0 is Al and 1 is Cu
% (this parameter is capital Phi in the journal article).
% This is related to eSF/eRGB, where eSF is the stacking-fault energy.
% Optionally, AlCu parameter is a string 'Al', 'Ni', 'Au', or 'Cu', which
% then sets all other parameters automatically.  You can call it with
% just this parameter if you wish.
%
% eRGB:  Energy of a "random" grain boundary in J/m^2
%
% There are some additional options that are still written into the
% function but are currently not used:
% par42Al:  The 42 dimensionless parameters for Al
%
% par42Cu:  The 42 dimensionless parameters for Cu
%
% Note a majority of the entries for par42Al and par42Cu are normally
% identical.
%
% All parameters have default values.  Defaults for par42Al and par42Cu are
% the values found by numerical fitting to the 388*4 boundaries.  
% eRGB and AlCuparameter default to the values for Cu.
%
% Optionally returns the numerical AlCuparameter so the user can read the
% default value for each element.

    if ~exist('eRGB','var') || isempty(eRGB),
        eRGB = 1.03669431227427;    % Value for Cu
    end

    if ~exist('AlCuparameter','var') || isempty(AlCuparameter),
        AlCuparameter = 1;  % Value for Cu
    end

    if ~exist('par42Al','var') || isempty(par42Al),
        par42Al = [0.405204179289160;0.738862004021890;0.351631012630026;2.40065811939667;1.34694439281655;0.352260396651516;0.602137375062785;1.58082498976078;0.596442399566661;1.30981422643602;3.21443408257354;0.893016409093743;0.835332505166333;0.933176738717594;0.896076948651935;0.775053293192055;0.391719619979054;0.782601780600192;0.678572601273508;1.14716256515278;0.529386201144101;0.909044736601838;0.664018011430602;0.597206897283586;0.200371750006251;0.826325891814124;0.111228512469435;0.664039563157148;0.241537262980083;0.736315075146365;0.514591177241156;1.73804335876546;3.04687038671309;1.48989831680317;0.664965104218438;0.495035051289975;0.495402996460658;0.468878130180681;0.836548944799803;0.619285521065571;0.844685390948170;1.02295427618256];
    end

    if ~exist('par42Cu','var') || isempty(par42Cu),
        par42Cu = [0.405204179289160;0.738862004021890;0.351631012630026;2.40065811939667;1.34694439281655;3.37892632736175;0.602137375062785;1.58082498976078;0.710489498577995;0.737834049784765;3.21443408257354;0.893016409093743;0.835332505166333;0.933176738717594;0.896076948651935;0.775053293192055;0.509781056492307;0.782601780600192;0.762160812499734;1.10473084066580;0.529386201144101;0.909044736601838;0.664018011430602;0.597206897283586;0.200371750006251;0.826325891814124;0.0226010533470218;0.664039563157148;0.297920289861751;0.666383447163744;0.514591177241156;1.73804335876546;2.69805148576400;1.95956771207484;0.948894352912787;0.495035051289975;0.301975031994664;0.574050577702240;0.836548944799803;0.619285521065571;0.844685390948170;0.0491040633104212];
    end

    if ischar(AlCuparameter),
        switch AlCuparameter
            case 'Ni'
                eRGB = 1.44532834613925;
                AlCuparameter = 0.767911805073948;
            case 'Al'
                eRGB = 0.547128733614891;
                AlCuparameter = 0;
            case 'Au'
                eRGB = 0.529912885175204;
                AlCuparameter = 0.784289766313152;
            case 'Cu'
                eRGB = 1.03669431227427;
                AlCuparameter = 1;
            otherwise
                error('Undefined element')
        end
    end

    par43 = [eRGB;(par42Al+AlCuparameter*(par42Cu-par42Al))];        
end




function en = weightedmeanenergy(geom100,geom110,geom111,pars)
% en = weightedmeanenergy(geom100,geom110,geom111,pars)
% Calculate the energy for a single grain boundary.
%
% Input variables geom100, geom110, and geom111 are each matrices with 4
% rows, giving the non-redundant representations of the boundary about each
% set of axes as generated by distances_to_set.m.  See the comments in that
% function for further information.  The rows are distance;ksi;eta;phi.
%
% Input variable pars is a 43-element vector as created by makeparvec.m.
% This specifies all of the parameters needed for the 5DOF energy function
% on a specific fcc metal.
%
% Return variable en is the energy in J/m^2.
%
% The physical relevance of the parameters is commented wherever they
% appear, in this function and in the setxxx functions.

% Pull out the parameters relevant to the weighting of the 100, 110, and
% 111 sets
    eRGB = pars(1); % The only dimensioned parameter.  The energy scale, set by the energy of a random boundary.
    d0100 = pars(2); % Maximum distance for the 100 set.  Also the distance scale for the rsw weighting function.
    d0110 = pars(3); % Same for the 110 set
    d0111 = pars(4); % Same for the 111 set
    weight100 = pars(5); % Weight for the 100 set, relative to the random boundary
    weight110 = pars(6); % Same for 110
    weight111 = pars(7); % Same for 111

    offset = 0.00001;  % Cutoff of weighting function at small d, purely for numerical purposes

    % The following three energy lists are in units of eRGB.
    e100    = set100(geom100,pars);
    e110    = set110(geom110,pars);
    e111    = set111(geom111,pars);

    d100 = geom100(1,:);
    d110 = geom110(1,:);
    d111 = geom111(1,:);

    % Now calculate the weights, in a manner designed to give an rsw-like
    % function of d.  Note it calculates a weight for every representation of
    % the boundary within each set.
    s100    = sin(pi/2*d100/d0100);
    s100(d100>d0100) = 1;   % Weight saturates at zero above d0
    s100(d100<offset*d0100) = offset*pi/2;  % Avoid calculation of NaN's, replace with something small but finite
    w100    = (1./(s100.*(1-0.5*log(s100)))-1)*weight100;

    s110    = sin(pi/2*d110/d0110);
    s110(d110>d0110) = 1;
    s110(d110<offset*d0110) = offset*pi/2;
    w110    = (1./(s110.*(1-0.5*log(s110)))-1)*weight110;

    s111    = sin(pi/2*d111/d0111);
    s111(d111>d0111) = 1;
    s111(d111<offset*d0111) = offset*pi/2;
    w111    = (1./(s111.*(1-0.5*log(s111)))-1)*weight111;

    en = eRGB*(sum(e100.*w100)+sum(e110.*w110)+sum(e111.*w111)+1)/(sum(w100)+sum(w110)+sum(w111)+1);
end


function en = set100(geom100,pars)
% en = set100(geom100,pars)
%
% Calculate the dimensionless contribution to the boundary based on the
% nearby <100> rotations.  Meant to be called by weightedmeanenergy.m, but
% also can be a stand-alone function for purposes of plotting cross
% sections through the function.
% Input variables geom100 and pars are as generated by distances_to_set.m
% and makeparvec.m.  See comments in those functions for more information.

    pwr1 = pars(8); % 100 tilt/twist mix power law:  Twist
    pwr2 = pars(9); % 100 tilt/twist mix power law:  Tilt

    ksi = geom100(2,:);
    eta = geom100(3,:);
    phi = geom100(4,:);

    entwist = twist100(ksi,pars) ;
    entilt  = atgb100(eta,ksi,pars) ;

    x = phi/(pi/2);
    en = entwist.*(1-x).^pwr1 + entilt.*x.^pwr2 ;

end



function en = twist100(ksi,pars)
% en = twist100(ksi,pars)
%
% Dimensionless 100 twist contribution to the energy

    a = pars(10);   % 100 twist maximum energy
    b = pars(10)*pars(11);  % 100 twist rsw shape factor. The unusual split into two parameters is a holdover from an older version.

    perio = pi/2 ;  % the twist period
    ksi = mod(abs(ksi),perio) ;      % rotation symmetry

    ksi(ksi > perio/2) = perio-ksi(ksi>perio/2);

    % Implement an rsw function of ksi
    sins = sin(2*ksi) ;
    xlogx = sins.*log(sins);
    xlogx(isnan(xlogx))=0; % Force the limit to zero as x -> 0.
    en =  a*sins - b*xlogx ;

end




function en = atgb100(eta,ksi,pars)
% en = atgb100(eta,ksi,pars)
%
% This function is a fit to the energies of all 100-tilt boundaries
%

    pwr = pars(12); % 100 atgb interpolation power law

    period = pi/2 ;

    en1 = stgb100(ksi,pars) ;  % Value at eta = 0
    en2 = stgb100(period-ksi,pars) ;  % Value at eta = pi/2

    % eta dependence is a power law that goes from the higher to the lower,
    % whichever direction that is
    select = en1>=en2;
    en = zeros(size(ksi));

    en(select) = en1(select) - (en1(select)-en2(select)).*(eta(select)/period).^pwr ;
    en(~select) = en2(~select) - (en2(~select)-en1(~select)).*(1-eta(~select)/period).^pwr ;
end


function en = stgb100(ksi,pars)
% en = stgb100(ksi,pars)
%
% dimensionless 100 symmetric tilt energy
%
% This is implemented as a piecewise-rsw function, specified by energy
% parameters en, angle breaks th, and shape factors a.

    en2 = pars(13); % peak before first Sigma5
    en3 = pars(14); % first Sigma5
    en4 = pars(15); % peak between Sigma5's
    en5 = pars(16); % second Sigma5
    en6 = pars(17); % Sigma17

    th2 = pars(18); % position of peak before first Sigma5
    th4 = pars(19); % position of peak between Sigma5's

    th6 = 2*acos(5/sqrt(34)); %Sigma17 rotation angle
    a12 = 0.5;  % rsw shape factor.  In previous versions, these were allowed
    a23 = a12;  % to vary, however there were too few vicinal boundaries in the
    a34 = a12;  % ensemble to constrain them.  We found that forcing the great
    a45 = a12;  % majority of them to be 0.5 helped to constrain the fit and 
    a56 = a12;  % produced reasonable results.  This holds true for most of the
    a67 = a12;  % rsw shape factors throughout this code.
    %

    en1 = 0 ; % Sigma1 at left end
    en7 = 0 ; % Sigma1 at right end

    th1 = 0 ; % Sigma1 at left end
    th3 = acos(4/5) ; % first Sigma5
    th5 = acos(3/5) ; % second Sigma5
    th7 = pi/2 ; % Sigma1 at right end
    %
    % And the rest is just the piecewise rsw function itself.
    en = zeros(size(ksi));
    select = (ksi<=th2);
        en(select) = en1 + (en2-en1)*rsw(ksi(select),th1,th2,a12) ;
    select = (ksi >= th2 & ksi <= th3);
        en(select) = en3 + (en2-en3)*rsw(ksi(select),th3,th2,a23) ;
    select = (ksi >= th3 & ksi <= th4 );
        en(select) = en3 + (en4-en3)*rsw(ksi(select),th3,th4,a34) ;
    select = (ksi >= th4 & ksi <= th5);
        en(select) = en5 + (en4-en5)*rsw(ksi(select),th5,th4,a45) ;
    select = (ksi >= th5 & ksi <= th6 );
        en(select) = en6 + (en5-en6)*rsw(ksi(select),th6,th5,a56) ;
    select = (ksi >= th6 & ksi <= th7);
        en(select) = en7 + (en6-en7)*rsw(ksi(select),th7,th6,a67) ;
end



function en = set110(geom110,pars)
% en = set110(geom110,pars)
%
% Dimensionless contribution to energy from <110> rotations
% Very similar to set100; see comments therein for general information.
% Comments in this file will be limited to 110-specific information.


    pwr1 = pars(20); % 110 tilt/twist mix power law:  Twist
    pwr2 = pars(21); % 110 tilt/twist mix power law:  Tilt

    ksi = geom110(2,:);
    eta = geom110(3,:);
    phi = geom110(4,:);

    %
    entwist = twists110(ksi,pars) ;
    entilt  = atgbs110(eta,ksi,pars) ;
    x = phi/(pi/2);
    en = entwist.*(1-x).^pwr1 + entilt.*x.^pwr2 ;
    end


function en = atgbs110(eta,ksi,pars)
% en = atgbs110(eta,ksi,pars)
% See comments on set110.

    a = pars(26); % 110 atgb interpolation rsw shape factor
    %
    period = pi ;
    en1 = stgbs110(ksi,pars) ;
    en2 = stgbs110(period-ksi,pars) ;

    en = zeros(size(eta));

    % Power-law interpolation did not work well in this case.  Did an rsw
    % function instead.
    select = en1>=en2;

    en(select) = en2(select) + (en1(select)-en2(select)).*rsw(eta(select),pi,0,a);
    en(~select) = en1(~select) + (en2(~select)-en1(~select)).*rsw(eta(~select),0,pi,a);

end


function en = stgbs110(th,pars)
% en = stgbs110(th,pars)
% See comments on set110.

    en2 = pars(27); % peak between Sigma1 and Sigma3
    en3 = pars(28); % Coherent Sigma3 twin relative energy; one of the more important element-dependent parameters
    en4 = pars(29); % energy peak between Sigma3 and Sigma11
    en5 = pars(30); % Sigma11 energy
    en6 = pars(31); % energy peak between Sigma11 and Sigma1

    th2 = pars(32); % peak between Sigma1 and Sigma3
    th4 = pars(33); % peak between Sigma3 and Sigma11
    th6 = pars(34); % peak between Sigma11 and higher Sigma1

    a12 = 0.5;
    a23 = 0.5;
    a34 = 0.5;
    a45 = 0.5;
    a56 = 0.5;
    a67 = 0.5;
    %
    % 
    en1 = 0 ;
    en7 = 0 ;

    th1 = 0 ;
    th3 = acos(1/3) ;  % Sigma3
    th5 = acos(-7/11) ; % Sigma11
    th7 = pi ;
    %
    th = pi - th ;  % This is a legacy of an earlier (ksi,eta) mapping 
    %
    en = zeros(size(th));

    select = th<=th2;
    en(select) = en1 + (en2-en1).*rsw(th(select),th1,th2,a12) ;

    select = th >= th2 & th <= th3;
    en(select) = en3 + (en2-en3).*rsw(th(select),th3,th2,a23) ;

    select = th >= th3 & th <= th4;
    en(select) = en3 + (en4-en3).*rsw(th(select),th3,th4,a34) ;

    select = th >= th4 & th <= th5;
    en(select) = en5 + (en4-en5).*rsw(th(select),th5,th4,a45) ;

    select = th >= th5 & th <= th6;
    en(select) = en5 + (en6-en5).*rsw(th(select),th5,th6,a56) ;

    select = th >= th6 & th <= th7;
    en(select) = en7 + (en6-en7).*rsw(th(select),th7,th6,a67) ;


end


function en = twists110(th,pars)
% en = twists110(th,pars)
%
% See comments on set110.

    th1 = pars(22); % 110 twist peak position

    en1 = pars(23); % 110 twist energy peak value
    en2 = pars(24); % Sigma3 energy (110 twist, so not a coherent twin)
    en3 = pars(25); % energy at the symmetry point

    a01 = 0.5;
    a12 = 0.5;
    a23 = 0.5;
    %
    th2 = acos(1/3) ; % Sigma3
    th3 = pi/2 ;  % 110 90-degree boundary is semi-special, although not a CSL

    perio = pi ;  % the twist period
    %
    th = mod(abs(th),perio) ;      % rotation symmetry
    th(th > perio/2) = perio - th(th > perio/2) ;

    en = zeros(size(th));
    %
    select = th <= th1;
    en(select) = en1*rsw(th(select),0,th1,a01) ;

    select = th > th1 & th <= th2;
    en(select) = en2 + (en1-en2)*rsw(th(select),th2,th1,a12) ;

    select = th > th2;
    en(select) = en3 + (en2-en3)*rsw(th(select),th3,th2,a23) ;

end

function en = set111(geom111,pars)
% en = set111(geom111,pars)
%
% Dimensionless contribution to energy from <111> rotations
% Very similar to set100; see comments therein for general information.
% Comments in this file will be limited to 111-specific information.

    a = pars(35); % linear part of 111 tilt/twist interpolation
    b = a - 1;  % Ensures correct value at x = 1.

    ksi = geom111(2,:);
    eta = geom111(3,:);
    phi = geom111(4,:);

    entwist = twists111(ksi,pars) ;
    entilt  = atgbs111(eta,ksi,pars) ;
    x       = phi/(pi/2); 

    % This one fit well enough with a simple one-parameter parabola that the
    % more complicated power laws in the other sets weren't needed.
    en      = entwist + (entilt - entwist).*(a*x - b*x.^2) ;
end



function en = twists111(theta,pars)
% en = twists111(theta,pars)
%
% See comments on set111

    thd = pars(37); % 111 twist peak position

    enm = pars(38); % 111 twist energy at the peak
    en2 = pars(28); % Coherent sigma3 twin shows up in two distinct places in the code

    a1 = pars(36); % 111 twist rsw shape parameter
    a2 = a1;

    theta(theta > pi/3) = 2*pi/3-theta(theta > pi/3);

    select = (theta<=thd);
    en = zeros(size(theta));
    en(select) = enm*rsw(theta(select),0,thd,a1) ;
    en(~select) = en2 + (enm - en2)*rsw(theta(~select),pi/3,thd,a2);
end


function en = atgbs111(eta,ksi,pars)
% en = atgbs111(eta,ksi,pars)
%
% This function is a fit to the energies of all 111-tilt boundaries

% There's an additional symmetry in 111 atgbs that doesn't exist in 100 or
% 110 atgbs.  This is because a rotation about [111] equal to half the period
% (i.e. 60 degrees) is equivalent to a mirror reflection in the (111)
% plane.  Both are Sigma3 operations.  The same is not true of the
% 45-degree [100] or the 90-degree [110] rotation.
% The following two lines account for this extra symmetry.
    ksi(ksi > pi/3) = 2*pi/3 - ksi(ksi>pi/3);
    eta(eta > pi/3) = 2*pi/3 - eta(eta>pi/3);

    % Below the following value of ksi, we ignore the eta dependence.  This is
    % because there's little evidence that it actually varies.  Above this
    % value, we interpolate on an rsw function that follows the Sigma3 line,
    % which is also a line of symmetry for the function.
    ksim  = pars(39); % 111 atgb ksi break

    enmax = pars(40); % Energy at the peak (ksi == ksim)
    enmin = pars(41); % energy at the minimum (sigma3, eta == 0)
    encnt = pars(42); % energy at the symmetry point (sigma3, eta == pi/3)

    a1    = 0.5;
    a2    = 0.5;

    etascale = pars(43); % eta scaling parameter for 111 atgb rsw function on Sigma3 line
        % This rsw function is unusual in that the change in shape of the
        % function is much better captured by changing the angular scale rather
        % than changing the dimensionless shape factor.

    en = zeros(size(ksi));

    select = (ksi <= ksim);
    en(select)  = enmax*rsw(ksi(select),0,ksim,a1) ;

    % chi is the shape of the function along the sigma3 line.
    chi = enmin + (encnt-enmin)*rsw(eta(~select),0,pi/(2*etascale),0.5);
    en(~select)  = chi   + (enmax - chi).*rsw(ksi(~select),pi/3,ksim,a2) ;
end

function en = rsw(theta,theta1,theta2,a)
% en = rsw(theta,theta1,theta2,a)
%
% This function computes the value of Read-Shockley-Wolf function at theta.
% The RSW function is normalized to be 1.0 at theta2 and 0.0 at theta1.
% 
% theta             angle at which to compute the function
% theta1            the starting angle of the interval
% theta2            the end angle of the interval
% a                 parameter defining the shape of the RSW function
%
    dtheta = theta2 - theta1 ;      % Interval of angles where defined
    theta = (theta-theta1)./dtheta*pi/2 ;    % Normalized angle
    % The rest is the RSW function evaluation
    sins = sin(theta) ;
    xlogx = zeros(size(sins));

    % Cut off at small sins to avoid 0*infinity problem.  The proper limit is 0.
    select = sins >= 0.000001;
    xlogx(select) = sins(select).*log(sins(select));

    en = sins - a*xlogx ;
end